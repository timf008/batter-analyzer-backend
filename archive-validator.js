
function validateArchive(snapshot) {
    const errors = [];

    // Verify archive structure
    if (!snapshot || typeof snapshot !== "object" || Array.isArray(snapshot)) {
        return {
            valid: false,
            errors: ["Archive must be a JSON object."]
        };
    }

    const metadata = snapshot.metadata;

    if (!metadata || typeof metadata !== "object" || Array.isArray(metadata)) {
        errors.push("Missing or invalid metadata.");
    } else {
        // Verify season
        if (!Number.isInteger(Number(metadata.season)) ||
            Number(metadata.season) < 1900 ||
            Number(metadata.season) > 2100) {
            errors.push("Invalid season.");
        }

        // Verify snapshot date and calendar validity
        const date = metadata.snapshotDate;
        const dateValid =
            typeof date === "string" &&
            /^\d{4}-\d{2}-\d{2}$/.test(date) &&
            !Number.isNaN(Date.parse(date)) &&
            new Date(date).toISOString().slice(0, 10) === date;

        if (!dateValid) {
            errors.push("Invalid snapshot date.");
        } else if (
            Number.isInteger(Number(metadata.season)) &&
            Number(date.slice(0, 4)) !== Number(metadata.season)
        ) {
            errors.push("Snapshot year does not match season.");
        }
    }

    // Verify player records
    if (!Array.isArray(snapshot.players)) {
        errors.push("Players must be an array.");
    } else {
        if (metadata &&
            Number(metadata.playerCount) !== snapshot.players.length) {
            errors.push("Player count does not match metadata.");
        }

        const identifiers = new Set();

        for (const [index, player] of snapshot.players.entries()) {
            if (!player || typeof player !== "object" || Array.isArray(player)) {
                errors.push(`Invalid player at index ${index}.`);
                continue;
            }

            const id = String(player.BBRefID ?? "").trim();

            if (!id) {
                errors.push(`Missing BBRefID at index ${index}.`);
            } else if (identifiers.has(id)) {
                errors.push(`Duplicate BBRefID: ${id}`);
            } else {
                identifiers.add(id);
            }
        }
    }

    return {
        valid: errors.length === 0,
        errors
    };
}

module.exports = { validateArchive };
