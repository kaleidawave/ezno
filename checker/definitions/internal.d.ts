declare function debug_context(): void performs const debug_context;
declare function print_type(): void performs const print_type;
declare function debug_type(): void performs const debug_type;
declare function debug_effects(): void performs const debug_effects;
declare function is_dependent(): void performs const is_dependent;

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

interface Math {
    sin(x: number): number performs const sin;
    cos(x: number): number performs const cos;
    tan(x: number): number performs const tan;
    trunc(x: number): number performs const trunc;
    sqrt(x: number): number performs const sqrt;
    cbrt(x: number): number performs const cbrt;
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