const fs = require("fs");

module.exports.get_cli_args = () => process.argv.slice(2).join('\0');
module.exports.read_from_path = (path) => fs.readFileSync(path).toString();
module.exports.read_from_cli = () => {
    throw "TODO"
};
