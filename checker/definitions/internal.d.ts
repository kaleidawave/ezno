declare function debug_context(): void performs const debug_context;
declare function print_type(): void performs const print_type;
declare function debug_type(): void performs const debug_type;
declare function debug_effects(): void performs const debug_effects;

interface Array<T> {
    length: number;

    [index: number]: T;

    push(item: any) performs {
        this[this.length] = item;
        return ++this.length
    }

    last() performs {
        return this[this.length - 1]
    }
}

type StringOrNumber = string | number;

interface Operators {
    Add<T extends StringOrNumber, U extends StringOrNumber>(a: T, b: U): (T extends string ? string : U extends string ? string: number) performs const add;

    Sub(a: number, b: number): number performs const sub;

    Mul(a: number, b: number): number performs const mul;

    Equal(a: any, b: any): boolean performs const equal;
}

interface Math {
    sin(x: number): number performs const sin;
    cos(x: number): number performs const cos;
    tan(x: number): number performs const tan;
}

interface string {
    toUppercase(): string performs const uppercase;
}

interface Console {
    log(msg: any): void;
}

interface JSON {
    // TODO any temp
    parse(input: string): any;

    // TODO any temp
    stringify(input: any): string;
}

declare var JSON: JSON;
declare var Math: Math;
declare var console: Console;