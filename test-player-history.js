
const { getPlayerHistory } = require("./player-history");

async function runTest() {
    try {
        const history = await getPlayerHistory("nonexistent999", 2026);

        console.log("TiME Machine: Player history lookup successful!");
        console.log("Historical records found:", history.length);

        for (const record of history) {
            console.log("---------------------------");
            console.log("Snapshot:", record.snapshotDate);
            console.log("Player:", record.player.Player);
            console.log("BBRefID:", record.player.BBRefID);
            console.log("Overall:", record.player.Overall);
            console.log("XP:", record.player.XP);
            console.log(
                "Park Adjusted Overall:",
                record.player.ParkAdjustedOverall
            );
        }

    } catch (error) {
        console.error(
            "TiME Machine: Player history lookup failed:",
            error.name,
            error.message
        );
        process.exitCode = 1;
    }
}

runTest();
