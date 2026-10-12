
const { GetObjectCommand } = require("@aws-sdk/client-s3");
const { r2 } = require("./r2-storage");

const archiveKey =
    "testing/batting/2026/batting-2026-10-11-hash-test.json";

async function testArchiveRead() {
    try {
        const response = await r2.send(
            new GetObjectCommand({
                Bucket: process.env.R2_BUCKET_NAME,
                Key: archiveKey
            })
        );

        const contents = await response.Body.transformToString();
        const archive = JSON.parse(contents);

        console.log("TiME Machine: Archive retrieved successfully!");
        console.log("Season:", archive.metadata?.season);
        console.log("Snapshot date:", archive.metadata?.snapshotDate);
        console.log("Player count:", archive.metadata?.playerCount);
        console.log("Archive version:", archive.metadata?.archiveVersion);
        console.log("Source hash:", archive.metadata?.sourceHash);

console.log("Top-level keys:", Object.keys(archive));

for (const [key, value] of Object.entries(archive)) {
    if (Array.isArray(value)) {
        console.log(`Array "${key}": ${value.length} records`);
    }
}

if (Array.isArray(archive.players) && archive.players.length > 0) {
    console.log(
        "First player fields:",
        Object.keys(archive.players[0])
    );
}

const ids = archive.players.map(p => p.BBRefID);

const missingIDs = ids.filter(
    id => id === null ||
          id === undefined ||
          String(id).trim() === ""
).length;

const validIDs = ids.filter(
    id => id !== null &&
          id !== undefined &&
          String(id).trim() !== ""
);

const uniqueIDs = new Set(validIDs.map(String));

console.log("Total players:", ids.length);
console.log("Missing BBRefIDs:", missingIDs);
console.log("Unique BBRefIDs:", uniqueIDs.size);
console.log("Duplicate BBRefIDs:", validIDs.length - uniqueIDs.size);

    } catch (error) {
        console.error(
            "TiME Machine: Archive retrieval failed:",
            error.name,
            error.message
        );
        process.exitCode = 1;
    }
}

testArchiveRead();
