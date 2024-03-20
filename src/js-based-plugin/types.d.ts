export type ReadFromFS = (path: string) => string | null;

export interface EznoUnpluginOptions {
    /** Defaults to only running on .ezno.* files */
    all_js_ts_files?: boolean,

    customBuild?: (cb: ReadFromFS, entryPath: string, minify: boolean) => any
}