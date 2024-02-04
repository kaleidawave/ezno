// ↓↓ Ezno Functions ↓↓
declare function debug_context(): void performs const debug_context;
declare function print_type(t: any): void performs const print_type;
declare function print_constraint(t: any): void performs const print_constraint;
declare function debug_type(t: any): void performs const debug_type;
declare function debug_type_independent(t: any): void performs const debug_type_independent;
declare function debug_type_rust(t: any): void performs const debug_type_rust;
declare function debug_type_rust_independent(t: any): void performs const debug_type_rust_independent;
declare function debug_effects_rust(t: () => {}): void performs const debug_effects_rust;
declare function debug_effects(t: () => {}): void performs const debug_effects;
declare function is_dependent(t: any): void performs const is_dependent;

declare function context_id(): void performs const context_id;
declare function context_id_chain(): void performs const context_id_chain;

// A function, as it should be!
declare function satisfies<T>(t: T): T performs const satisfies;

declare function compile_type_to_object<T>(): any performs const compile_type_to_object;
// ↑↑ Ezno Functions ↑↑

// declare var undefined: undefined;

interface nominal Array<T> {
    [index: number]: T | undefined;
    
    length: number;

    push(item: T) performs {
        this[this.length] = item;
        return ++this.length
    }

    // pop(): T | undefined performs {
    //     if (this.length === 0) {
    //         return undefined
    //     } else {
    //         const value = this[--this.length];
    //         delete this[this.length];
    //         return value
    //     }
    // }

    // TODO this argument
    map<U>(cb: (t: T, i?: number) => U): Array<U> performs {
        const { length } = this, u: Array<U> = [];
        let i: number = 0;
        while (i < length) {
            const value = this[i];
            u.push(cb(value, i++))
        }
        return u;
    }

    map2<U>(cb: (t: T, i?: number) => U): any performs {
        return cb;
    }

    // last() performs {
    //     return this[this.length - 1]
    // }
}

interface Math {
    @DoNotIncludeThis
    sin(x: number): number performs const sin;
    @DoNotIncludeThis
    cos(x: number): number performs const cos;
    @DoNotIncludeThis
    tan(x: number): number performs const tan;
    @DoNotIncludeThis
    floor(x: number): number performs const floor;
    @DoNotIncludeThis
    sqrt(x: number): number performs const sqrt;
    @DoNotIncludeThis
    cbrt(x: number): number performs const cbrt;

    // TODO newer method
    trunc(x: number): number performs const trunc;

    PI: 3.141592653589793
}

interface nominal string {
    [index: number]: string | undefined;

    toUpperCase(): string performs const uppercase;
    toLowerCase(): string performs const lowercase;

    get length(): number performs const string_length;
}

interface Console {
    @DoNotIncludeThis
    log(msg: any): void;
}

interface JSON {
    // TODO any temp
    @DoNotIncludeThis
    parse(input: string): any;

    // TODO any temp
    @DoNotIncludeThis
    stringify(input: any): string;
}

interface Function {
    bind(this_ty: any): Function performs const bind;
}

interface Symbols {
    // TODO temp
    iterator: 199
}

declare const Symbol: Symbols;

interface Object {
    @DoNotIncludeThis
    setPrototypeOf(on: object, to: object): object performs const set_prototype;

    @DoNotIncludeThis
    getPrototypeOf(on: object): object | null performs const get_prototype;

    // create(prototype: object): object performs {
    //     const n = {};
    //     Object.setProtoTypeOf(n, prototype);
    //     return n
    // }
}

declare const JSON: JSON;
declare const Math: Math;
declare const console: Console;
declare const Object: Object;

declare function JSXH(tag: string, attributes: any, children?: any) performs {
    return { tag, attributes, children }
}

interface Document {
    title: string
}

interface FormData {
}

@client
declare const document: Document;

// @server
// declare function createItem(a: any);