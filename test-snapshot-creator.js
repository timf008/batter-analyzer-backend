
const { PutObjectCommand } = require("@aws-sdk/client-s3");
const { r2 } = require("./r2-storage");

const snapshot = {
    metadata: {
        season: 2026,
        snapshotDate: "2026-09-01",
        playerCount: 1,
        archiveVersion: "synthetic-test-1",
        dataType: "synthetic"
    },
    players: [
        {
            Player: "Aaron Judge",
            BBRefID: "judgeaa01",
            Overall: 7.25,
            XP: 1190,
            ParkAdjustedOverall: 7.10
        }
    ]
};

const ARCHIVE_KEY =
    "testing/batting/2026/batting-2026-09-01-synthetic.json";

async function createTestSnapshot() {
    try {
        const { validateArchive } = require("./archive-validator");

        const validation = validateArchive(snapshot);

        if (!validation.valid) {
            throw new Error(validation.errors.join("; "));
        }

        await r2.send(
            new PutObjectCommand({
                Bucket: process.env.R2_BUCKET_NAME,
                Key: ARCHIVE_KEY,
                Body: JSON.stringify(snapshot, null, 2),
                ContentType: "application/json",
                IfNoneMatch: "*"
            })
        );

        console.log("TiME Machine: Synthetic snapshot uploaded!");
        console.log("Archive:", ARCHIVE_KEY);
        console.log("Date:", snapshot.metadata.snapshotDate);
        console.log("Players:", snapshot.metadata.playerCount);
    } catch (error) {
        console.error(
            "TiME Machine: Snapshot creation failed:",
            error.name,
            error.message
        );
        process.exitCode = 1;
    }
}

createTestSnapshot();
