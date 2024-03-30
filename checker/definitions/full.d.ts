@Constant
declare function debug_type_independent(t: any): void;

declare class Array<T> {
    [index: number]: T | undefined;

    length: number;

    push(this: Array<T>, item: T) {
        this[this.length] = item;
        return ++this.length
    }

    pop(this: Array<T>, ): T | undefined {
        if (this.length === 0) {
            return undefined
        } else {
            const value = this[--this.length];
            delete this[this.length];
            return value
        }
    }

    // TODO this argument
    map<U>(this: Array<T>, cb: (t: T, i?: number) => U): Array<U> {
        const { length } = this, mapped: Array<U> = [];
        let i: number = 0;
        while (i < length) {
            const value = this[i];
            mapped.push(cb(value, i++))
        }
        return mapped;
    }

    // // TODO any is debatable
    filter(this: Array<T>, cb: (t: T, i?: number) => any): Array<T> {
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
    find(this: Array<T>, cb: (t: T, i?: number) => any): T | undefined {
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
    every(this: Array<T>, cb: (t: T, i?: number) => any): boolean {
        const { length } = this;
        let i: number = 0;
        while (i < length) {
            const value = this[i];
            if (!cb(value, i++)) {
                return false
            }
        }
        // Vacuous truth
        return true
    }

    some(this: Array<T>, cb: (t: T, i?: number) => any): boolean {
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

    // includes(searchElement: T, fromIndex?: number): boolean {
    //     const { length } = this;
    //     let i: number = fromIndex ?? 0;
    //     while (i < length) {
    //         const value = this[i++];
    //         if (value === searchElement) {
    //             return true
    //         }
    //     }
    //     return false
    // }

    join(this: Array<T>, joiner: string = ","): string {
        const { length } = this;
        let i: number = 1;
        if (length === 0) {
            return ""
        }
        let s: string = "" + this[0];
        while (i < length) {
            s += joiner;
            s += this[i++];
            // debug_type_independent(s)
        }
        return s
    }
}

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

    // TODO newer method
    @Constant
    static trunc(x: number): number;

    static PI: 3.141592653589793
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

interface Response {
    ok: boolean;

    json(): Promise<any>;
}

declare class Console {
    @InputOutput
    log(msg: any): void;
}

declare const console: Console;

declare class JSON {
    // TODO any temp
    parse(input: string): any;

    // TODO any temp
    stringify(input: any): string;
}

declare class Function {
    bind(this_ty: any): Function;
}

declare class Symbols {
    // TODO temp
    iterator: 199
}

declare class Object {
    @Constant
    static setPrototypeOf(on: object, to: object): object;

    @Constant
    static getPrototypeOf(on: object): object | null;

    // create(prototype: object): object performs {
    //     const n = {};
    //     Object.setProtoTypeOf(n, prototype);
    //     return n
    // }

    // keys(on: object): Array<string> performs {
    //     const array = [];
    //     for (const key in on) {
    //         array.push(key);
    //     }
    //     return array
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
declare function print_type(t: any): void;
@Constant
declare function debug_type(t: any): void;
@Constant
declare function print_and_debug_type(t: any): void;
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

// A function, as it should be!
@Constant
declare function satisfies<T>(t: T): T;

@Constant
declare function compile_type_to_object<T>(): any;
// ↑↑ Ezno Functions ↑↑
