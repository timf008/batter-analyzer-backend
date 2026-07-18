const express = require("express");
const cors = require("cors");
const path = require("path");
const { exec } = require("child_process");
const fs = require("fs");

const app = express();

// ---------------------------
// GLOBAL CORS (must be first)
// ---------------------------
app.use(cors());
app.use((req, res, next) => {
    res.setHeader("Access-Control-Allow-Origin", "*");
    next();
});

// ---------------------------
// Name Normalization (Latin accents → ASCII)
// ---------------------------
function normalizeNameBackend(x) {
    return x
        .normalize("NFKD")
        .replace(/[\u0300-\u036f]/g, "")
        .replace(/[^\w\s-]/g, "")
        .replace(/\s+/g, " ")
        .trim()
        .toUpperCase();
}


// ---------------------------
// Safe Rscript wrapper with timeout
// ---------------------------
function runR(cmd, timeoutMs = 8000) {
    return new Promise((resolve) => {
        exec(cmd, { timeout: timeoutMs }, (error, stdout) => {
            if (error) {
                console.error("R crashed or timed out:", error);
                return resolve(null);
            }
            resolve(stdout);
        });
    });
}

// ===========================================================
// API ROUTES — MUST COME BEFORE STATIC FILES
// ===========================================================

// ---------------------------
// API: Run R script for **batting** data
// ---------------------------
app.get("/api/batters", async (req, res) => {
    let { name, season } = req.query;

    if (!name || !season) {
        return res.status(400).json({ error: "Missing name or season" });
    }

    name = normalizeNameBackend(name);
    console.log("Normalized name sent to R:", name);

    const cmd = `cd "${__dirname}" && Rscript "stathead_batting.r" "${name}" "${season}"`;
    const output = await runR(cmd);

    if (!output) {
        return res.status(500).json({ error: "R timeout or crash" });
    }

    try {
        const json = JSON.parse(output);
        return res.json(json);
    } catch (e) {
        console.error("JSON parse error:", e);
        console.log("Raw R output:", output);
        return res.status(500).json({ error: "Invalid JSON from R" });
    }
});

// ---------------------------
// Rscript wrapper for leaders / compare / trend
// ---------------------------
const { spawn } = require("child_process");

function runRScript(scriptName, args = []) {
    return new Promise((resolve, reject) => {
        const child = spawn("Rscript", [scriptName, ...args], {
            cwd: __dirname
        });

        let output = "";
        let errorOutput = "";

        child.stdout.on("data", (data) => {
            output += data.toString();
        });

        child.stderr.on("data", (data) => {
            errorOutput += data.toString();
        });

        child.on("close", (code) => {
            if (code !== 0) {
                return reject(new Error(errorOutput));
            }
            resolve(output);
        });
    });
}


// --------------------------------------
// API: Leaders Button
// --------------------------------------
app.get("/api/leaders", async (req, res) => {
    try {
        const season = req.query.season;

        if (!season) {
            return res.status(400).json({ error: "Season required" });
        }

        const result = await runRScript("leaders.r", [season]);
        const data = JSON.parse(result);

        res.json(data);

    } catch (err) {
        console.error("Leaders API error:", err);
        res.status(500).json({ error: "Server error" });
    }
});



// --------------------------------------
// API: Last Updated timestamp for batting CSV
// --------------------------------------
app.get("/api/last-updated/batters/:season", (req, res) => {
    const season = req.params.season;
    const filePath = path.join(__dirname, `stathead_batting_${season}.csv`);

    fs.stat(filePath, (err, stats) => {
        if (err) {
            console.error("Timestamp error:", err);
            return res.status(404).json({ error: `CSV for season ${season} not found` });
        }

        return res.json({
            season,
            lastUpdated: stats.mtime
        });
    });
});

// ===========================================================
// STATIC FILES — MUST COME LAST
// ===========================================================
app.use(express.static(path.join(__dirname, "public")));

// ---------------------------
// Start Server
// ---------------------------
const PORT = process.env.PORT || 3000;
app.listen(PORT, () => {
    console.log(`Batter Analyzer running at http://localhost:${PORT}`);
});


