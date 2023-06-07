interface nominal Array<T> {
    @PropertyId(10)
    length: number,

    // [n: number]: T | undefined,

    @InternalFunctionId(4)
    toString(): string;

    @InternalFunctionId(101)
    toLocaleString(): string;

    @Events("calls F with T")
    forEach<F extends (v: T) => void>(cb: F): void;

    @InternalFunctionId(11)
    push(...items: T[]): number;

    @InternalFunctionId(12)
    pop(): T | undefined;

    @InternalFunctionId(10)
    @Events("calls F with T")
    map<U, F extends (v: T) => U>(cb: F): Array<U>;

    // @Events("calls F with T")
    @InternalFunctionId(12)
    reduce<U, F extends (previousValue: U, currentValue: T, currentIndex: number, array: readonly T[]) => U>(callbackFn: F, initialValue: U): U;

    @InternalFunctionId(14)
    at(index: number): T | undefined;

    // TODO array copy thingy

    // TODO I think the RHS should be a function here:
    // [Symbol.iterator]: T;
}

interface nominal string {
    // length: ℕ,
    length: number,

    @InternalFunctionId(101)
    toUpperCase(): string;
    @InternalFunctionId(102)
    toLowerCase(): string;

    @InternalFunctionId(4)
    toString(): string;

    @InternalFunctionId(5)
    valueOf(): self;
    
    @InternalFunctionId(103)
    slice(start: number, end?: number);

    @InternalFunctionId(104)
    split(separator: string): string[];
}

interface nominal number {
    @InternalFunctionId(4)
    toString(): string;

    @InternalFunctionId(5)
    valueOf(): self;
}

interface nominal boolean {
    @InternalFunctionId(5)
    valueOf(): self;
}

interface Console {
    @InternalFunctionId(600)
    log(...args: any[]): void;
}

interface HTMLElement {
    @InternalFunctionId(200)
    constructor(): HTMLElement;

    // TODO should also be `| null`
    onclick: () => any

    append(...elements: HTMLElement[]): void;

    // TODO do these exist on HTMLElement
    classList: DOMTokenList;
    innerHTML: string;

    childNodes: NodeList;
}

interface NodeList { }

interface Node { }

interface CharacterData extends Node {
    data: string
}

interface Text extends CharacterData {
    // TODO toString
    // @Effects("sets 'data' on self to D")
    // constructor<D>(data: D): { data: D } & self;
}

// @Internal
// @InternalFunctionId(100)
// @TypeId(100)
// @Effects(
//     "if T extends string | number",
//     "return calls new Text with T",
//     "else",
//     "return T"
// )
// declare function newNode<T>() -> SPECIAL_RESULT

interface DOMTokenList {
    length: number;
    add(...tokens: string[]): void;
}

interface HTMLHeadingElement extends HTMLElement {}
interface HTMLParagraphElement extends HTMLElement {}
interface HTMLSpanElement extends HTMLElement {}
interface HTMLDivElement extends HTMLElement {}
interface HTMLButtonElement extends HTMLElement { 
    @InternalFunctionId(0)
    click(): void; 
}
interface HTMLFormElement extends HTMLElement {
    action: string,

    @InternalFunctionId(0)
    reset(): void;
    @InternalFunctionId(0)
    submit(): void;
}

interface HTMLStyleElement extends HTMLElement { }
interface HTMLImageElement extends HTMLElement {
    src: string
}
interface HTMLBodyElement extends HTMLElement { }

declare var console: readonly Console;

interface Object {
    @InternalFunctionId(90)
    freeze(object: any);
}

interface Document {
    @InternalFunctionId(151)
    append(...element: HTMLElement[]): void;
    
    @InternalFunctionId(152)
    querySelector(query: string): HTMLElement;
    
    @InternalFunctionId(153)
    createDocumentFragment(): DocumentFragment;
    
    @InternalFunctionId(154)
    createElement(tagName: string): HTMLElement;

    body: HTMLBodyElement;
}

interface DocumentFragment { }

interface HTMLElementTagNameMap {
    // "a": HTMLAnchorElement;
    // "abbr": HTMLElement;
    // "address": HTMLElement;
    // "applet": HTMLAppletElement;
    // "area": HTMLAreaElement;
    // "article": HTMLElement;
    // "aside": HTMLElement;
    // "audio": HTMLAudioElement;
    // "b": HTMLElement;
    // "base": HTMLBaseElement;
    // "basefont": HTMLBaseFontElement;
    // "bdi": HTMLElement;
    // "bdo": HTMLElement;
    // "blockquote": HTMLQuoteElement;
    "body": HTMLBodyElement;
    // "br": HTMLBRElement;
    "button": HTMLButtonElement;
    // "canvas": HTMLCanvasElement;
    // "caption": HTMLTableCaptionElement;
    // "cite": HTMLElement;
    // "code": HTMLElement;
    // "col": HTMLTableColElement;
    // "colgroup": HTMLTableColElement;
    // "data": HTMLDataElement;
    // "datalist": HTMLDataListElement;
    // "dd": HTMLElement;
    // "del": HTMLModElement;
    // "details": HTMLDetailsElement;
    // "dfn": HTMLElement;
    // "dialog": HTMLDialogElement;
    // "dir": HTMLDirectoryElement;
    "div": HTMLDivElement;
    // "dl": HTMLDListElement;
    // "dt": HTMLElement;
    // "em": HTMLElement;
    // "embed": HTMLEmbedElement;
    // "fieldset": HTMLFieldSetElement;
    // "figcaption": HTMLElement;
    // "figure": HTMLElement;
    // "font": HTMLFontElement;
    // "footer": HTMLElement;
    "form": HTMLFormElement;
    // "frame": HTMLFrameElement;
    // "frameset": HTMLFrameSetElement;
    "h1": HTMLHeadingElement;
    "h2": HTMLHeadingElement;
    "h3": HTMLHeadingElement;
    "h4": HTMLHeadingElement;
    "h5": HTMLHeadingElement;
    "h6": HTMLHeadingElement;
    // "head": HTMLHeadElement;
    // "header": HTMLElement;
    // "hgroup": HTMLElement;
    // "hr": HTMLHRElement;
    // "html": HTMLHtmlElement;
    // "i": HTMLElement;
    // "iframe": HTMLIFrameElement;
    // "img": HTMLImageElement;
    // "input": HTMLInputElement;
    // "ins": HTMLModElement;
    // "kbd": HTMLElement;
    // "label": HTMLLabelElement;
    // "legend": HTMLLegendElement;
    // "li": HTMLLIElement;
    // "link": HTMLLinkElement;
    // "main": HTMLElement;
    // "map": HTMLMapElement;
    // "mark": HTMLElement;
    // "marquee": HTMLMarqueeElement;
    // "menu": HTMLMenuElement;
    // "meta": HTMLMetaElement;
    // "meter": HTMLMeterElement;
    // "nav": HTMLElement;
    // "noscript": HTMLElement;
    // "object": HTMLObjectElement;
    // "ol": HTMLOListElement;
    // "optgroup": HTMLOptGroupElement;
    // "option": HTMLOptionElement;
    // "output": HTMLOutputElement;
    // "p": HTMLParagraphElement;
    // "param": HTMLParamElement;
    // "picture": HTMLPictureElement;
    // "pre": HTMLPreElement;
    // "progress": HTMLProgressElement;
    // "q": HTMLQuoteElement;
    // "rp": HTMLElement;
    // "rt": HTMLElement;
    // "ruby": HTMLElement;
    // "s": HTMLElement;
    // "samp": HTMLElement;
    // "script": HTMLScriptElement;
    // "section": HTMLElement;
    // "select": HTMLSelectElement;
    // "slot": HTMLSlotElement;
    // "small": HTMLElement;
    // "source": HTMLSourceElement;
    // "span": HTMLSpanElement;
    // "strong": HTMLElement;
    // "style": HTMLStyleElement;
    // "sub": HTMLElement;
    // "summary": HTMLElement;
    // "sup": HTMLElement;
    // "table": HTMLTableElement;
    // "tbody": HTMLTableSectionElement;
    // "td": HTMLTableDataCellElement;
    // "template": HTMLTemplateElement;
    // "textarea": HTMLTextAreaElement;
    // "tfoot": HTMLTableSectionElement;
    // "th": HTMLTableHeaderCellElement;
    // "thead": HTMLTableSectionElement;
    // "time": HTMLTimeElement;
    // "title": HTMLTitleElement;
    // "tr": HTMLTableRowElement;
    // "track": HTMLTrackElement;
    // "u": HTMLElement;
    // "ul": HTMLUListElement;
    // "var": HTMLElement;
    // "video": HTMLVideoElement;
    // "wbr": HTMLElement;
}

// TODO temp, requires @invariant
declare var document: readonly Document;

interface ArrayConstructor {
    // new(arrayLength?: number): any[];
    @InternalFunctionId(704)
    pick<T>(items: T[]): T performs {
        return items[Math.randomRangeInt(0, items.length - 1)]
    }


}

declare var Array: ArrayConstructor;

@Events("calls Func with undefined at some point")
declare function setTimeout<Func extends () => void>(params: Func): void;

@This
interface Window {
    onload: () => void;
}
declare var window: Window;
// TODO temp
// declare var customElements: readonly CustomElementRegistry;

/// 500 -> 550
interface Math {
    @InternalFunctionId(501)
    imul(a: number, b: number): number;

    // TODO not 0-1
    @InternalFunctionId(502)
    random(): number;

    @InternalFunctionId(503)
    sin(x: number): number;
    @InternalFunctionId(504)
    cos(x: number): number;
    @InternalFunctionId(505)
    tan(x: number): number;
    @InternalFunctionId(506)
    asin(x: number): number;
    @InternalFunctionId(507)
    acos(x: number): number;
    @InternalFunctionId(508)
    atan(x: number): number;
    
    @InternalFunctionId(509)
    floor(x: number): number;
    @InternalFunctionId(510)
    ceil(x: number): number;
    @InternalFunctionId(511)
    round(x: number): number;
    @InternalFunctionId(512)
    trunc(x: number): number;

    @InternalFunctionId(513)
    sqrt(x: number): number;

    @InternalFunctionId(520)
    min(a: number, b: number): number;
    @InternalFunctionId(521)
    max(a: number, b: number): number;

    @InternalFunctionId(705)
    between(x: number, a: number, b: number): number performs ("ES2025") {
        return x >= a && x <= b;
    }

    @InternalFunctionId(706)
    randomRange(min: number, max: number): number performs ("ES2025") {
        return Math.round(Math.random() * (max - min) + min)
    }
    @InternalFunctionId(707)
    randomRangeInt(a: number, b: number): number performs ("ES2025") {
        return Math.round(Math.randomRange(min, max))
    }

    @InternalFunctionId(523)
    pow(a: number, b: number): number;

    PI: 3.141592653589793;
}

declare var Math: readonly Math;

// declare function fetch(url: string): Promise<Response>;

interface Function {
    /* This is always constant called */
    @InternalFunctionId(303)
    call(t: any, ...args: any[]): any;
    
    /* TODO any 
    @InternalFunctionId(304)
    bind(t: any): any;
    */
}

// interface FunctionConstructor {
//     @InternalFunctionId(0)
//     new (content: string): Function;
// }

// declare var Function: FunctionConstructor;

@InternalFunctionId(707)
declare function prompt(prompt?: string): string;
@InternalFunctionId(708)
declare function parseFloat(number: string): number;



// React stuff
interface ReactRoot<T extends HTMLElement> {
    element: T,

    @Events("sets 'children' on T with U")
    render<U>(node: U): undefined;
}

declare function createRoot(element: T): ReactRoot<T>;

// TODO not sure about the Ts here, maybe using TypeScripts new invariant thing
@InternalFunctionId(221)
declare function useState<T>(initialState: T): [T, (newState: T) => void];

// @InternalFunctionId(222)
// declare function useRef<T>(initialRef: T): JSXRef<T>;

interface ReactDOM {
    // TODO element can also be class component...
    @InternalFunctionId(201)
    render(element: (props?: any) => HTMLElement, container: HTMLElement): void;
}

declare var ReactDOM: ReactDOM;


interface CustomElementRegistry {
    // TODO extends HTMLElementConstructor
    // @Events("registers ce T under N")
    @InternalFunctionId(210)
    define<N extends string, T >(name: N, constructor: T): void;

    // define(name: string, constructor: CustomElementConstructor, options?: ElementDefinitionOptions): void;
    // get(name: string): any;
    // upgrade(root: Node): void;
    // whenDefined(name: string): Promise<void>;
}

declare var CustomElementRegistry: readonly CustomElementRegistry;
// declare var CustomElementRegistry: {
//     prototype: CustomElementRegistry;
//     new(): CustomElementRegistry;
// };


// Internal functions
@InternalFunctionId(800)
declare function assertType<T>(t: T);
@InternalFunctionId(801)
declare function printEffects(func: Function);


@InternalFunctionId(569)
declare function debugEnvironment();
@InternalFunctionId(570)
declare function debug(val: any);

interface ProxyConstructor { 
    @InternalFunctionId(580)
    new(object: object, trap: object): any;
}

declare var Proxy: ProxyConstructor;

// interface Map<K, V> {
//     // TODO this actually returns self... hmm
//     @InternalFunctionId(0)
//     set(key: K, value: V): Map<K, V>;

//     @InternalFunctionId(0)
//     hash(key: K): bool;

//     @InternalFunctionId(0)
//     get(key: K): V;
// }


// interface Promise<T> { }

// // All from https://github.com/microsoft/TypeScript/blob/d699bcdaae9d7138eb29612d410e628154a254b6/src/lib/es2015.promise.d.ts
// // PromiseLike condenses/collapses T
// interface PromiseConstructor {
//     new <T>(executor: (
//         resolve: (value: T) => void, // | PromiseLike<T>
//         reject: (reason?: any) => void
//     ) => void): Promise<T>;

//     resolve<T>(value: T): Promise<T>; // | PromiseLike<T>
// }

// declare var Promise: PromiseConstructor;