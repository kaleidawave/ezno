interface nominal Array<T> {
    [index: number]: T | undefined;
    
    length: number;

    push(item: T) performs {
        this[this.length] = item;
        return ++this.length
    }

    pop(): T | undefined performs {
        if (this.length === 0) {
            return undefined
        } else {
            const value = this[--this.length];
            delete this[this.length];
            return value
        }
    }

    // TODO this argument
    map<U>(cb: (t: T, i?: number, self: Array<T>) => U): Array<U> performs {
        const { length } = this, u: Array<U> = [];
        let i: number = 0;
        while (i < length) {
            const value = this[i];
            u.push(cb(value, i++, this))
        }
        return u;
    }

    filter(cb: (t: T, i?: number, self: Array<T>) => boolean): Array<T> performs {
        const { length } = this, filtered: Array<T> = [];
        let i: number = 0;
        while (i < length) {
            const value = this[i++];
            if (cb(value)) {
                filtered.push(value)
            }
        }
        return filtered;
    }

    find(cb: (t: T, i?: number, self: Array<T>) => boolean): T | undefined performs {
        const { length } = this;
        let i: number = 0;
        while (i < length) {
            const value = this[i++];
            if (cb(value)) {
                return value
            }
        }
    }

    every(cb: (t: T, i?: number, self: Array<T>) => boolean): boolean performs {
        const { length } = this;
        let i: number = 0;
        while (i < length) {
            const value = this[i++];
            if (!cb(value)) {
                return false
            }
        }
        // Vacuous truth
        return true
    }

    some(cb: (t: T, i?: number, self: Array<T>) => boolean): boolean performs {
        const { length } = this;
        let i: number = 0;
        while (i < length) {
            const value = this[i++];
            if (cb(value)) {
                return true
            }
        }
        return false
    }

    includes(searchElement: T, fromIndex?: number): boolean performs {
        const { length } = this;
        let i: number = fromIndex ?? 0;
        while (i < length) {
            const value = this[i++];
            if (value === searchElement) {
                return true
            }
        }
        return false
    }

    join(joiner: string = ","): string performs {
        const { length } = this;
        let i = 1;
        if (length === 0) {
            return ""
        }
        // TODO conversion
        let s: string = "" + this[0];
        while (i < length) {
            s += this[i++];
        }
        return s
    }
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

    // TODO
    slice(start: number, end?: number): string performs const slice;

    // TODO
    split(splitter: string): Array<string> performs const split;
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

declare var Symbol: Symbols;

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

declare var JSON: JSON;
declare var Math: Math;
declare var console: Console;
declare var Object: Object;

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