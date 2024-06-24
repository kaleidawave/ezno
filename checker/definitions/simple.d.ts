// This contains some of definitions from overrides. Some can be removed during development to test things (and to remove things that might be causing a crash)

@Constant
declare function print_type<T>(...args: Array<T>): void
@Constant
declare function debug_type<T>(...args: Array<T>): void
@Constant
declare function debug_type_independent<T>(...args: Array<T>): void
@Constant
declare function print_and_debug_type<T>(...args: Array<T>): void
@Constant
declare function print_constraint(t: any): void
@Constant
declare function debug_type_rust(t: any): void
@Constant
declare function debug_type_rust_independent(t: any): void

@Constant
declare function debug_effects_rust(t: () => {}): void
@Constant
declare function debug_effects(t: () => {}): void

@Constant
declare function is_dependent(t: any): void
@Constant
declare function print_environment_state<T>(): any

@Constant
declare function debug_context(): void
@Constant
declare function context_id(): void
@Constant
declare function context_id_chain(): void

// A function, as it should be!
@Constant
declare function satisfies<T>(t: T): T

@Constant
declare function compile_type_to_object<T>(): any

interface ImportEnv {
    [key: string]: string | undefined;
}

interface ImportMeta {
    env: ImportEnv;
    url: string;
    resolve(url: string): string;
}

declare class Array<T> {
    [index: number]: T | undefined;

    length: number;

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

    // // TODO this argument
    // map<U>(cb: (t: T, i?: number) => U): Array<U> {
    //     const { length } = this, mapped: Array<U> = [];
    //     let i: number = 0;
    //     while (i < length) {
    //         const value = this[i];
    //         mapped.push(cb(value, i++))
    //     }
    //     return mapped;
    // }

    // // // TODO any is debatable
    // filter(cb: (t: T, i?: number) => any): Array<T> {
    //     const { length } = this, filtered: Array<T> = [];
    //     let i: number = 0;
    //     while (i < length) {
    //         const value = this[i];
    //         if (cb(value, i++)) {
    //             filtered.push(value)
    //         }
    //     }
    //     return filtered;
    // }

    // // TODO any is debatable
    // find(cb: (t: T, i?: number) => any): T | undefined {
    //     const { length } = this;
    //     let i: number = 0;
    //     while (i < length) {
    //         const value = this[i];
    //         if (cb(value, i++)) {
    //             return value
    //         }
    //     }
    // }

    // // TODO any is debatable
    // every(cb: (t: T, i?: number) => any): boolean {
    //     const { length } = this;
    //     let i: number = 0;
    //     while (i < length) {
    //         const value = this[i];
    //         if (!cb(value, i++)) {
    //             return false
    //         }
    //     }
    //     // Vacuous truth
    //     return true
    // }

    // includes(looking_for: T): boolean {
    //     const { length } = this;
    //     let i: number = 0;
    //     while (i < length) {
    //         const value = this[i++];
    //         if (value === looking_for) {
    //             return true
    //         }
    //     }
    //     return false
    // }

    // some(cb: (t: T, i?: number) => any): boolean {
    //     const { length } = this;
    //     let i: number = 0;
    //     while (i < length) {
    //         const value = this[i];
    //         if (cb(value, i++)) {
    //             return true
    //         }
    //     }
    //     return false
    // }

    // join(joiner: string = ","): string {
    //     const { length } = this;
    //     let i: number = 1;
    //     if (length === 0) {
    //         return ""
    //     }
    //     let s: string = "" + this[0];
    //     while (i < length) {
    //         s += joiner;
    //         s += this[i++];
    //     }
    //     return s
    // }

    // at(index: number) {
    //     if (index < 0) {
    //         return this[index + this.length]
    //     } else {
    //         return this[index]
    //     }
    // }
}

declare class Map<K, V> {
    #keys: Array<K>;
    #values: Array<V>;

    constructor() {
        this.#keys = []
        this.#values = []
    }

    get(key: K): V | undefined {
        // return this.#keys;
        const { length } = this.#keys;
        for (let i = 0; i < length; i++) {
            const s = length - 1 - i;
            if (this.#keys[s] === key) {
                return this.#values[s]
            }
        }
    }

    set(key: K, value: V) {
        this.#keys.push(key);
        this.#values.push(value);
    }
}

type Record<K extends string, T> = { [P in K]: T }

/**
 * Exclude from T those types that are assignable to U
 */
type Exclude<T, U> = T extends U ? never : T;

/**
 * Extract from T those types that are assignable to U
 */
type Extract<T, U> = T extends U ? T : never;

declare class Math {
    @Constant
    static sin(x: number): number;
    @Constant
    static cos(x: number): number;
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

    static PI: 3.141592653589793
    static E: 2.718281828459045
}

@Primitive("string")
declare class String {
    [index: number]: string | undefined;

    @Constant
    toUpperCase(): string;
    @Constant
    toLowerCase(): string;

    get length(): number;

    // TODO
    slice(start: number, end?: number): string;

    // TODO
    split(splitter: string): Array<string>;
}

declare class Promise<T> { }

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
    @InputOutput
    log(...data: any[]): void;
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

declare class Symbol {
    // TODO temp
    static iterator: unique symbol "iterator"
}

declare class Proxy {
    @Constant("proxy:constructor")
        constructor(obj: any, cb: any);
}

declare class Object {
    @Constant
    static setPrototypeOf(on: object, to: object): object;

    @Constant
    static getPrototypeOf(on: object): object | null;

    // static create(prototype: object): object {
    //     const n = {};
    //     Object.setProtoTypeOf(n, prototype);
    //     return n
    // }

    // static keys(on: { [s: string]: any }): Array<string> {
    //     const keys: Array<string> = [];
    //     for (const key in on) {
    //         keys.push(key);
    //     }
    //     return keys
    // }

    // static values(on: { [s: string]: any }): Array<any> {
    //     const values: Array<any> = [];
    //     for (const key in on) {
    //         values.push(on[key]);
    //     }
    //     return values
    // }

    // static entries(on: { [s: string]: any }): Array<[string, any]> {
    //     const entries: Array<[string, any]> = [];
    //     for (const key in on) {
    //         entries.push([key, on[key]]);
    //     }
    //     return entries
    // }

    // static fromEntries(iterator: any): object {
    //     const obj = {};
    //     for (const item of iterator) {
    //         const { 0: key, 1: value } = item;
    //         obj[key] = value;
    //     }
    //     return obj
    // }

    // static assign(iterator: any): object {
    // }
}

declare class RegExp {
    @Constant("RegExp:constructor")
        constructor(s: string)
}

// WIP
// interface SymbolImplementations {
//     [Symbol.iterator]: () => { next(): { value: any, done: boolean } }
// }

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
