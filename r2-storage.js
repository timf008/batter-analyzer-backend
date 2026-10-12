
const {
    S3Client,
    HeadBucketCommand
} = require("@aws-sdk/client-s3");

// Cloudflare R2 connection
const r2 = new S3Client({
    region: "auto",
    endpoint: `https://${process.env.R2_ACCOUNT_ID}.r2.cloudflarestorage.com`,
    credentials: {
        accessKeyId: process.env.R2_ACCESS_KEY_ID,
        secretAccessKey: process.env.R2_SECRET_ACCESS_KEY
    }
});

// Read-only connection test
async function testR2Connection() {
    try {
        await r2.send(
            new HeadBucketCommand({
                Bucket: process.env.R2_BUCKET_NAME
            })
        );

        console.log("TiME Machine: R2 connection successful!");
        return true;

    } catch (error) {
        console.error(
            "TiME Machine: R2 connection failed:",
            error.name,
            error.message
        );
        return false;
    }
}

module.exports = {
    r2,
    testR2Connection
};
