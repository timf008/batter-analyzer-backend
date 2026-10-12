
const { GetObjectCommand } = require("@aws-sdk/client-s3");
const { r2 } = require("./r2-storage");
const { validateArchive } = require("./archive-validator");

const TEST_ARCHIVE_KEY =
    "testing/batting/2026/batting-2026-10-11-hash-test.json";

async function runTest() {
    try {
        const response = await r2.send(
            new GetObjectCommand({
                Bucket: process.env.R2_BUCKET_NAME,
                Key: TEST_ARCHIVE_KEY
            })
        );

        const contents = await response.Body.transformToString();
        const snapshot = JSON.parse(contents);

        const result = validateArchive(snapshot);

        console.log("TiME Machine: Archive validation completed!");
        console.log("Archive:", TEST_ARCHIVE_KEY);
        console.log("Valid:", result.valid);
        console.log("Errors:", result.errors.length);

        for (const error of result.errors) {
            console.log("Validation error:", error);
        }

        if (!result.valid) {
            process.exitCode = 1;
        }
    } catch (error) {
        console.error(
            "TiME Machine: Archive validation failed:",
            error.name,
            error.message
        );
        process.exitCode = 1;
    }
}

runTest();
