const fs = require("fs");

/**
 * Converts all the shorthand declarations in a piece of text
 * to its full variable declaration.
 * @param {string} data
 * @returns {string}
 */
function convertToFull(data) {
    const lines = data.split(/\n/);
    const replacedLines = lines.map((line) => {
        if (line.includes(": ")) return line;
        return line.replace(/\w+\s\:\=/, (match) => {
            const [variable] = match.split(/\s/);
            const result = `var ${variable} =`
            return result;
        });
    })
    return replacedLines.join("\n")
}

function convertFile(path) {
    const fileData = fs.readFileSync(path).toString();
    const converted = convertToFull(fileData);
    fs.writeFileSync(path, converted);
}

const [path] = process.argv.slice(2)
convertFile(path);
