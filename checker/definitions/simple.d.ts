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

    push(item: T) {
        let at = this.length++;
        this[at] = item;
        return at + 1
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

    // map<U>(cb: (t: T, i?: number) => U): Array<U> {
    // TODO this argument
    map<U>(cb: (t: T, i?: number) => U): Array<U> {
        const { length } = this, mapped: Array<U> = [];
        let i: number = 0;
        while (i < length) {
            const value = this?.[i];
            const newValue = cb(value, i++);
            mapped.push(newValue)
        }
        return mapped;
    }

    // copy(): Array<T> {
    //     const { length } = this, mapped: Array<T> = [];
    //     let i: number = 0;
    //     while (i < length) {
    //         mapped.push(this?.[i])
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

    // get(key: K): V | undefined {
    //     // return this.#keys;
    //     const { length } = this.#keys;
    //     for (let i = 0; i < length; i++) {
    //         const s = length - 1 - i;
    //         if (this.#keys[s] === key) {
    //             return this.#values[s]
    //         }
    //     }
    // }

    // set(key: K, value: V) {
    //     this.#keys.push(key);
    //     this.#values.push(value);
    // }
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

    @InputOutput
    static random(): number;
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
    constructor() {
        super("syntax error");
    }
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
    static isFrozen(on: object): boolean;

    // TODO defineProperties via body (not constant)
    @Constant
    static defineProperty(on: object, property: string, discriminator: PropertyDescriptor): boolean;

    // TODO getOwnPropertyDescriptors via body (not constant)
    @Constant
    static getOwnPropertyDescriptor(on: object, property: string): PropertyDescriptor;

    // static create(prototype: object): object {
    //     const n = {};
    //     Object.setProtoTypeOf(n, prototype);
    //     return n
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
