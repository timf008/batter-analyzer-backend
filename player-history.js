
const { GetObjectCommand } = require("@aws-sdk/client-s3");
const { r2 } = require("./r2-storage");
const { listHistoricalArchives } = require("./historical-archives");
const { validateArchive } = require("./archive-validator");

// Retrieve a player's history across all test snapshots.
async function getPlayerHistory(bbrefID, season) {
    const playerID = String(bbrefID ?? "").trim();

    if (!playerID) {
        throw new Error("A valid BBRefID is required.");
    }

    const archives = await listHistoricalArchives(season);
    const history = [];

    for (const archiveInfo of archives) {
        const response = await r2.send(
            new GetObjectCommand({
                Bucket: process.env.R2_BUCKET_NAME,
                Key: archiveInfo.key
            })
        );

        const contents = await response.Body.transformToString();
        const snapshot = JSON.parse(contents);

// Validate archive before accessing player records
const validation = validateArchive(snapshot);

if (!validation.valid) {
    throw new Error(
        `Archive validation failed: ${archiveInfo.key}. ` +
        validation.errors.join("; ")
    );
}

        const player = snapshot.players.find(
            p => String(p.BBRefID) === playerID
        );

        if (player) {
            history.push({
                snapshotDate: snapshot.metadata?.snapshotDate,
                archiveKey: archiveInfo.key,
                player: player
            });
        }
    }

    // Sort by actual snapshot date, not upload date.
    history.sort((a, b) =>
        String(a.snapshotDate ?? "").localeCompare(
            String(b.snapshotDate ?? "")
        )
    );

    return history;
}

module.exports = { getPlayerHistory };
