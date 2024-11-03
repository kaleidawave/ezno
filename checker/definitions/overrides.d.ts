// Eventually this will be merged with existing TS es5.d.ts files but for now is the standalone see #121

interface ImportEnv {
    [key: string]: string;
}

interface ImportMeta {
    env: ImportEnv;
    url: string;
    resolve(url: string): string;
}

declare class Array<T> {
    [index: number]: T;

    length: number;

    // TODO WIP
    // constructor(...items: Array<T>) {
    //     return items;
    // }

    push(item: T) {
        this[this.length] = item;
        return ++this.length
    }

    pop(): T | undefined {
        if (this.length === 0) {
            return undefined
        } else {
            const value = this[--this.length];
            delete this[this.length];
            return value
        }
    }

    // TODO this argument
    map<U>(cb: (t: T, i?: number) => U): Array<U> {
        const { length } = this, mapped: Array<U> = [];
        let i: number = 0;
        while (i < length) {
            const value = this[i];
            mapped.push(cb(value, i++))
        }
        return mapped;
    }

    // // TODO any is debatable
    filter(cb: (t: T, i?: number) => any): Array<T> {
        const { length } = this, filtered: Array<T> = [];
        let i: number = 0;
        while (i < length) {
            const value = this[i];
            if (cb(value, i++)) {
                filtered.push(value)
            }
        }
        return filtered;
    }

    // TODO any is debatable
    find(cb: (t: T, i?: number) => any): T | undefined {
        const { length } = this;
        let i: number = 0;
        while (i < length) {
            const value = this[i];
            if (cb(value, i++)) {
                return value
            }
        }
    }

    // TODO any is debatable
    every(cb: (t: T, i?: number) => any): boolean {
        const { length } = this;
        let i: number = 0;
        while (i < length) {
            const value = this[i];
            const result = cb(value, i++) as boolean;
            if (!result) {
                return false
            }
        }
        // Vacuous truth
        return true
    }

    some(cb: (t: T, i?: number) => any): boolean {
        const { length } = this;
        let i: number = 0;
        while (i < length) {
            const value = this[i];
            if (cb(value, i++)) {
                return true
            }
        }
        return false
    }

    join(joiner: string = ","): string {
        const { length } = this;
        let i: number = 1;
        if (length === 0) {
            return ""
        }
        let s: string = "" + (this[0] as string);
        while (i < length) {
            s += joiner;
            s += this[i++] as string;
        }
        return s
    }

    at(index: number) {
        if (index < 0) {
            return this[index + this.length]
        } else {
            return this[index]
        }
    }

    static isArray(item: any) {
        return item instanceof Array;
    }
}

type Record<K extends string, T> = { [P in K]: T }

type LessThan<T extends number> = ExclusiveRange<NegativeInfinity, T>;
type GreaterThan<T extends number> = ExclusiveRange<T, Infinity>;
type Integer = MultipleOf<1>;

declare class Map<T, U> {
    #keys: Array<T> = [];
    #value: Array<T> = [];
}

declare class Math {
    @Constant
    static sin(x: number): InclusiveRange<-1, 1>;
    @Constant
    static cos(x: number): InclusiveRange<-1, 1>;
    @Constant
    static tan(x: number): number;
    @Constant
    static floor(x: number): number;
    @Constant
    static sqrt(x: number): number;
    @Constant
    static cbrt(x: number): number;
    @Constant
    static log(x: number): number;

    // TODO newer method
    @Constant
    static trunc(x: number): number;

    @Constant
    static imul(x: number, y: number): number;

    static PI: 3.141592653589793
    static E: 2.718281828459045

    @InputOutput
    static random(): InclusiveRange<0, 1>;
}

@Primitive("string")
declare class String {
    [index: number]: string;

    @Constant
    toUpperCase(this: string): string;
    @Constant
    toLowerCase(this: string): string;

    get length(): number;

    // TODO
    slice(start: number, end?: number): string;

    // TODO
    split(splitter: string): Array<string>;
}

@Primitive("number")
declare class Number {
    static NEGATIVE_INFINITY: NegativeInfinity;
    static POSITIVE_INFINITY: Infinity;

    // static isFinite(item: any) {
    //     return !(item === Number.NEGATIVE_INFINITY || item === Number.POSITIVE_INFINITY || Number.isNaN(item))
    // }

    static isNaN(item: any) {
        return item !== item;
    }
}

declare class Promise<T> { }

declare class RegExp {
    @Constant("regexp:constructor")
    constructor(pattern: string, flags?: string);

    @Constant("regexp:exec")
    exec(input: string): RegExpExecArray | null;
}

// es5
interface RegExpExecArray extends Array<string> {
    /**
     * The index of the search at which the result was found.
     */
    index: number;
    /**
     * A copy of the search string.
     */
    input: string;
    /**
     * The first match. This will always be present because `null` will be returned if there are no matches.
     */
    0: string;
}

// es2018
interface RegExpExecArray {
    groups?: {
        [key: string]: string;
    };
}

type ResponseBody = string;

declare class Response {
    ok: boolean;

    // constructor(body?: ResponseBody, options: any);

    // json(): Promise<any>;

    // static json(data: any): Response {
    //     return new Response(JSON.stringify(data))
    // }
}

declare class Console {
    /** [MDN Reference](https://developer.mozilla.org/docs/Web/API/console/assert_static) */
    @InputOutput
    assert(condition?: boolean, ...data: any[]): void;
    /** [MDN Reference](https://developer.mozilla.org/docs/Web/API/console/clear_static) */
    @InputOutput
    clear(): void;
    /** [MDN Reference](https://developer.mozilla.org/docs/Web/API/console/count_static) */
    @InputOutput
    count(label?: string): void;
    /** [MDN Reference](https://developer.mozilla.org/docs/Web/API/console/countReset_static) */
    @InputOutput
    countReset(label?: string): void;
    /** [MDN Reference](https://developer.mozilla.org/docs/Web/API/console/debug_static) */
    @InputOutput
    debug(...data: any[]): void;
    /** [MDN Reference](https://developer.mozilla.org/docs/Web/API/console/dir_static) */
    @InputOutput
    dir(item?: any, options?: any): void;
    /** [MDN Reference](https://developer.mozilla.org/docs/Web/API/console/dirxml_static) */
    @InputOutput
    dirxml(...data: any[]): void;
    /** [MDN Reference](https://developer.mozilla.org/docs/Web/API/console/error_static) */
    @InputOutput
    error(...data: any[]): void;
    /** [MDN Reference](https://developer.mozilla.org/docs/Web/API/console/group_static) */
    @InputOutput
    group(...data: any[]): void;
    /** [MDN Reference](https://developer.mozilla.org/docs/Web/API/console/groupCollapsed_static) */
    @InputOutput
    groupCollapsed(...data: any[]): void;
    /** [MDN Reference](https://developer.mozilla.org/docs/Web/API/console/groupEnd_static) */
    @InputOutput
    groupEnd(): void;
    /** [MDN Reference](https://developer.mozilla.org/docs/Web/API/console/info_static) */
    @InputOutput
    info(...data: any[]): void;
    /** [MDN Reference](https://developer.mozilla.org/docs/Web/API/console/log_static) */
    @InputOutput
    log(...data: any[]): void;
    /** [MDN Reference](https://developer.mozilla.org/docs/Web/API/console/table_static) */
    @InputOutput
    table(tabularData?: any, properties?: string[]): void;
    /** [MDN Reference](https://developer.mozilla.org/docs/Web/API/console/time_static) */
    @InputOutput
    time(label?: string): void;
    /** [MDN Reference](https://developer.mozilla.org/docs/Web/API/console/timeEnd_static) */
    @InputOutput
    timeEnd(label?: string): void;
    /** [MDN Reference](https://developer.mozilla.org/docs/Web/API/console/timeLog_static) */
    @InputOutput
    timeLog(label?: string, ...data: any[]): void;
    @InputOutput
    timeStamp(label?: string): void;
    /** [MDN Reference](https://developer.mozilla.org/docs/Web/API/console/trace_static) */
    @InputOutput
    trace(...data: any[]): void;
    /** [MDN Reference](https://developer.mozilla.org/docs/Web/API/console/warn_static) */
    @InputOutput
    warn(...data: any[]): void;
}

declare const console: Console;

declare class Error {
    message: string

    // TODO `@AllowElidedNew`
    constructor(message: string) {
        this.message = message
    }
}

declare class SyntaxError extends Error {
    constructor() { super("syntax error") }
}

declare class JSON {
    // TODO any temp
    @Constant("JSON:parse", SyntaxError)
    static parse(input: string): any;

    // TODO any temp
    @Constant("JSON:stringify")
    static stringify(input: any): string;
}

declare class Function {
    bind(this_ty: any): Function;
}

declare class Symbols {
    // TODO temp
    static iterator: unique symbol "iterator"
}

declare class Proxy {
    @Constant("proxy:constructor")
        constructor(obj: any, cb: any);
}

// Copied from `es5.d.ts`. Could this be an or
// TODO string keys temp because parser broke
interface PropertyDescriptor {
    value?: any;
    get?(): any;
    set?(v: any): void;

    writable?: boolean;
    configurable?: boolean;
    enumerable?: boolean;
}

declare class Object {
    @Constant
    static setPrototypeOf(on: object, to: object): object;

    @Constant
    static getPrototypeOf(on: object): object | null;

    @Constant
    static freeze(on: object): object;

    @Constant
    static seal(on: object): object;

    @Constant
    static preventExtensions(on: object): object;

    @Constant
    static isFrozen(on: object): boolean;

    @Constant
    static isSealed(on: object): boolean;

    @Constant
    static isExtensible(on: object): boolean;

    // TODO defineProperties via body (not constant)
    @Constant
    static defineProperty(on: object, property: string, discriminator: PropertyDescriptor): boolean;

    // TODO getOwnPropertyDescriptors via body (not constant)
    @Constant
    static getOwnPropertyDescriptor(on: object, property: string): PropertyDescriptor;

    static keys(on: { [s: string]: any }): Array<string> {
        const keys: Array<string> = [];
        for (const key in on) {
            keys.push(key);
        }
        return keys
    }

    static values(on: { [s: string]: any }): Array<any> {
        const values: Array<any> = [];
        for (const key in on) {
            values.push(on[key]);
        }
        return values
    }

    static entries(on: { [s: string]: any }): Array<[string, any]> {
        const entries: Array<[string, any]> = [];
        for (const key in on) {
            entries.push([key, on[key]]);
        }
        return entries
    }

    // TODO spread source
    static assign(target: object, source: object): object {
        for (const key in source) {
            target[key] = source[key]
        }
        return target
    }

    // static fromEntries(iterator: any): object {
    //     const obj = {};
    //     for (const item of iterator) {
    //         const { 0: key, 1: value } = item;
    //         obj[key] = value;
    //     }
    //     return obj
    // }
}

// TODO wip
declare function JSXH(tag: string, attributes: any, children?: any) {
    return { tag, attributes, children }
}

interface Document {
    title: string
}

interface FormData {
}

// TODO temp
@InputOutput
declare function fetch(from: string): Promise<Response>;

@client
declare const document: Document;

// @server
// declare function createItem(a: any);

// ↓↓ Ezno testing functions ↓↓
@Constant
declare function print_type<T>(...args: Array<T>): void;
@Constant
declare function debug_type<T>(...args: Array<T>): void;
@Constant
declare function print_and_debug_type<T>(...args: Array<T>): void;
@Constant
declare function print_constraint(t: any): void;
@Constant
declare function debug_type_rust(t: any): void;
@Constant
declare function debug_type_rust_independent(t: any): void;

@Constant
declare function debug_effects_rust(t: () => {}): void;
@Constant
declare function debug_effects(t: () => {}): void;

@Constant
declare function is_dependent(t: any): void;
@Constant
declare function print_environment_state<T>(): any;

@Constant
declare function debug_context(): void;
@Constant
declare function context_id(): void;
@Constant
declare function context_id_chain(): void;
@Constant
declare function debug_type_independent(t: any): void;

// A function, as it should be!
@Constant
declare function satisfies<T>(t: T): T;

@Constant
declare function compile_type_to_object<T>(): any;
// ↑↑ Ezno Functions ↑↑
