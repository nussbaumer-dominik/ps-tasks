class Token {
    constructor(type, text, startPos) {
        this.type = type;
        this.text = text;
        this.startPos = startPos;
    }
}

export { Token };
