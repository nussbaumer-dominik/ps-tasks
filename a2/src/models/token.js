class Token {
    constructor(tpe, text, startPos) {
        this.tpe = tpe;
        this.text = text;
        this.startPos = startPos;
    }
}

module.exports = Token;
