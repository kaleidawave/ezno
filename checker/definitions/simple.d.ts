@Constant
declare function print_type<T>(...args: Array<T>): void
@Constant
declare function debug_type<T>(...args: Array<T>): void
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
    map<U>(cb: (t: T, i?: number) => U): Array<U>;
}