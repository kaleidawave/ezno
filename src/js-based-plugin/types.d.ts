export type ReadFromFS = (path: string) => string | null;

export interface EznoUnpluginOptions {
    /** Defaults to only running on .ezno.* files */
    allJSFiles: bool,

    customBuild?: (cb: ReadFromFS, entryPath: string, minify: bool) => any
}