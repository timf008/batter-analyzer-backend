
const {
    listHistoricalArchives
} = require("./historical-archives");

async function runTest() {
    try {
        const archives = await listHistoricalArchives(2026);

        console.log("TiME Machine: Archive discovery successful!");
        console.log("Archives found:", archives.length);

        for (const archive of archives) {
            console.log("---------------------------");
            console.log("Key:", archive.key);
            console.log("Size:", archive.size, "bytes");
            console.log("Last modified:", archive.lastModified);
        }

    } catch (error) {
        console.error(
            "TiME Machine: Archive discovery failed:",
            error.name,
            error.message
        );
        process.exitCode = 1;
    }
}

runTest();
