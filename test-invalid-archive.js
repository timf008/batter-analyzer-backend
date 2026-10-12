
const { validateArchive } = require("./archive-validator");

const invalidSnapshot = {
    metadata: {
        season: 2026,
        snapshotDate: "2026-10-11",
        playerCount: 2
    },
    players: [
        {
            Player: "Test Player One",
            BBRefID: "test001"
        },
        {
            Player: "Test Player Two",
            BBRefID: "test001"
        }
    ]
};

const result = validateArchive(invalidSnapshot);

console.log("TiME Machine: Invalid archive test completed!");
console.log("Valid:", result.valid);
console.log("Errors:", result.errors.length);

for (const error of result.errors) {
    console.log("Validation error:", error);
}

if (result.valid) {
    console.error("TEST FAILED: Invalid archive was accepted.");
    process.exitCode = 1;
} else {
    console.log("TEST PASSED: Invalid archive was rejected.");
}
