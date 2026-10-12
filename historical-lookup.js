
const { GetObjectCommand } = require("@aws-sdk/client-s3");
const { r2 } = require("./r2-storage");

// Our verified test archive
const TEST_ARCHIVE_KEY =
    "testing/batting/2026/batting-2026-10-11-hash-test.json";

// Retrieve one historical player by BBRefID
async function getHistoricalPlayer(bbrefID) {
    if (typeof bbrefID !== "string" || !bbrefID.trim()) {
        throw new Error("A valid BBRefID is required.");
    }

    const response = await r2.send(
        new GetObjectCommand({
            Bucket: process.env.R2_BUCKET_NAME,
            Key: TEST_ARCHIVE_KEY
        })
    );

    const contents = await response.Body.transformToString();
    const archive = JSON.parse(contents);

    if (!Array.isArray(archive.players)) {
        throw new Error("Invalid archive: players array missing.");
    }

    const player = archive.players.find(
        p => String(p.BBRefID) === bbrefID.trim()
    );

    if (!player) {
        return null;
    }

    return {
        metadata: archive.metadata,
        player: player
    };
}

module.exports = { getHistoricalPlayer };
