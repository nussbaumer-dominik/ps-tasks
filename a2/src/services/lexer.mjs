import {Token} from "../models/token.mjs";
import {Type} from "../models/type.mjs";


const wordBreaks = [' ', '(', ')', '{', '}', ',', '[', ']', ':', '=', '>', '<', '+', '-', '*', '/', '%', '#'];
const simpleTokenMapping = {
    "(": Type.LPAREN,
    ")": Type.RPAREN,
    "{": Type.LCURLY,
    "}": Type.RCURLY,
    "[": Type.LSQUARE,
    "]": Type.RSQUARE,
    ",": Type.COMMA,
}

/**
 * Tokenizes a single line
 * @param input
 * @returns {Token[]} - A Tokenized Version of the input
 */
function lex(input) {
    if (input === undefined) {
        return [];
    }

    let tokens = [];
    let currentPos = 0;

    lineLoop:
    while (currentPos < input.length) {
        let char = input[currentPos];

        // ignore whitespace
        if (char === ' ') {
            currentPos++;
            continue;
        }

        // There are no closing comment tags, so just strip the whole line
        if (char === '#') {
            tokens.push(new Token(Type.COMMENT, input.substring(currentPos, input.length), currentPos));
            currentPos = input.length;
            continue;
        }

        // Lex simple one character tokens
        for (let [character, type] of Object.entries(simpleTokenMapping)) {
            if(char === character) {
                tokens.push(new Token(type, char, currentPos));
                currentPos++;
                continue lineLoop;
            }
        }

        // check for assignment :=
        if (char === ':' && currentPos + 1 < input.length ) {
            if (input[currentPos + 1] !== '=') {
                throw new Error("Invalid token " + char + input[currentPos + 1]);
            }
            tokens.push(new Token(Type.ASSIGN, ':=', currentPos));
            currentPos = currentPos + 2;
            continue;
        }

        // check for function lambda =>
        if (char === '=' && currentPos + 1 < input.length) {
            if (input[currentPos + 1] !== '>') {
                throw new Error("Invalid token " + char + input[currentPos + 1]);
            }
            tokens.push(new Token(Type.LAMBDA, '=>', currentPos));
            currentPos = currentPos + 2;
            continue;
        }

        // No specific Type found --> it has to be a word or number
        let wordLength = 1;
        let word = input[currentPos];
        while (currentPos + wordLength <= input.length) {
            //check if the word ends
            const nextChar = input[currentPos + wordLength];
            if (wordBreaks.includes(nextChar) || currentPos + wordLength === input.length) {
                tokens.push(new Token(getType(word), word, currentPos));
                currentPos += wordLength;
                break;
            }
            word += input[currentPos + wordLength];
            wordLength++;
        }
    }
    tokens.push(new Token(Type.EOL, "<EOL>", input.length))
    return tokens
}

/**
 * Checks if a 'word' is a number or an entity
 * @param word
 * @returns {Type} - Number or Entity
 */
function getType(word) {
    //check if word is a number
    if (!isNaN(word)) {
        return Type.NUMBER;
    }
    return Type.ENTITY;
}


export default lex;
