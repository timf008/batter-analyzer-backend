
const { testR2Connection } = require("./r2-storage");

(async () => {
    const connected = await testR2Connection();

    if (!connected) {
        process.exitCode = 1;
    }
})();
