
const { getHistoricalPlayer } = require("./historical-lookup");

// Replace this with an actual BBRefID from our archive.
const PLAYER_ID = "judgeaa01";

async function runTest() {
    try {
        const result = await getHistoricalPlayer(PLAYER_ID);

        if (!result) {
            console.log("TiME Machine: Player not found.");
            return;
        }

        console.log("TiME Machine: Historical player found!");
        console.log("Player:", result.player.Player);
        console.log("BBRefID:", result.player.BBRefID);
        console.log("Snapshot:", result.metadata.snapshotDate);
        console.log("Overall:", result.player.Overall);
        console.log("XP:", result.player.XP);
        console.log(
            "Park Adjusted Overall:",
            result.player.ParkAdjustedOverall
        );

    } catch (error) {
        console.error(
            "TiME Machine: Historical lookup failed:",
            error.message
        );
        process.exitCode = 1;
    }
}

runTest();
