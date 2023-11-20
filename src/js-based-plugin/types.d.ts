export type ReadFromFS = (path: string) => string | null;

export interface EznoUnpluginOptions {
    /** Defaults to only running on .ezno.* files */
    all_js_ts_files?: bool,

    customBuild?: (cb: ReadFromFS, entryPath: string, minify: bool) => any
}