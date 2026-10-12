
const {
    ListObjectsV2Command
} = require("@aws-sdk/client-s3");

const { r2 } = require("./r2-storage");

// Discover test archives for a season.
// No uploads, modifications, or deletions.
async function listHistoricalArchives(season) {
    const year = String(season);

    if (!/^\d{4}$/.test(year)) {
        throw new Error("A valid four-digit season is required.");
    }

    const prefix = `testing/batting/${year}/`;
    const archives = [];

    let continuationToken;

    do {
        const response = await r2.send(
            new ListObjectsV2Command({
                Bucket: process.env.R2_BUCKET_NAME,
                Prefix: prefix,
                ContinuationToken: continuationToken
            })
        );

        for (const object of response.Contents || []) {
            if (object.Key?.endsWith(".json")) {
                archives.push({
                    key: object.Key,
                    size: object.Size,
                    lastModified: object.LastModified
                });
            }
        }

        continuationToken = response.NextContinuationToken;
    } while (continuationToken);

    return archives.sort((a, b) =>
        a.key.localeCompare(b.key)
    );
}

module.exports = { listHistoricalArchives };
