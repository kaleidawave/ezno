import { createUnplugin } from "unplugin";
import { build } from "ezno/initialized";

function emitDiagnostics(on, diagnostics, plugin) {
	const lineSplits = [0];
	for (const line of on.split("\n")) {
		// TODO need buffer length not character count
		lineSplits.push(lineSplits.at(-1) + line.length + 1)
	}
	for (const diagnostic of diagnostics) {
		const line = lineSplits.findIndex(count => count >= diagnostic.position.start);
		const column = diagnostic.position.start - lineSplits[line - 1] + 1;
		// Unfortunately don't get to set an end point, level or any labels
		plugin.warn(diagnostic.label, { line, column })
	}
}

export default createUnplugin((_options) => {
	return {
		name: "ezno",
		transformInclude(id) {
			const extension = id.split(".").at(-1);
			return ["ts", "tsx", "js", "jsx"].includes(extension);
		},
		transform(code, id) {
			function resolver(path) {
				console.error(`tried to read path '${path}' which is currently unsupported by the plugin`)
				return "ERROR";
			}
			const output = build(resolver, code, id);
			if (output.Ok) {
				emitDiagnostics(code, output.Ok.temp_warnings_and_infos, this)
				return {
					code: output.Ok.outputs[0].content
				}
			} else {
				emitDiagnostics(code, output.Err, this)
				this.warn("ezno had errors and did not transform");
				return code;
			}
		},
	};
});
