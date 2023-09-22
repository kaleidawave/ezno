import { createUnplugin } from "unplugin";
import { build } from "ezno/initialized";

/// <reference path="types.d.ts"/>

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

/** @param {import("./types").EznoUnpluginOptions} options  */
function plugin(options) {
	let allFiles = options.all_files ?? false;
	// TODO the other 50
	const extensions = ["ts", "tsx", "js", "jsx"];

	return {
		name: "ezno",
		transformInclude(id) {
			const extension = id.split(".");
			const jsTsLikeExtension = extensions.includes(extension.at(-1));
			if (allFiles) {
				return jsTsLikeExtension;
			} else {
				return jsTsLikeExtension && extension.at(-2) == "ezno";
			}
		},
		transform(code, id) {
			/** Passed to Ezno's builder so it can import more */
			function resolver(path) {
				if (path !== id) {
					console.error(`tried to read another path '${path}' which is currently unsupported by the plugin`)
					return "ERROR";
				} else {
					return code;
				}
			}

			const output = build(resolver, id);
			if (output.Ok) {
				emitDiagnostics(code, output.Ok.diagnostics, this)
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
}

export default createUnplugin(plugin);
