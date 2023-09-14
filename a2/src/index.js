#!/usr/bin/env node
import fs from 'fs';
import readline from 'readline';
import lex from './services/lexer.mjs';
import parse from "./services/parser.mjs";
import interpret from "./services/interpreter.mjs";

//1. read file from disc

const filePath = 'examples/demo-code.txt';
//const filePath = 'examples/loop.txt';
//const filePath = 'examples/avg.txt';
//const filePath = 'examples/isPrime.txt';

// Create a readable stream from the file
const fileStream = fs.createReadStream(filePath);

// Create a readline interface
const rl = readline.createInterface({
    input: fileStream,
    crlfDelay: Infinity // Recognize all instances of CR LF ('\r\n') as a single line break
});

const lexResult = [];
// Event handler for each line
rl.on('line', (line) => {
    // console.log(`Line from file: ${line}`);
    lexResult.push(...lex(line));
});

// Event handler for when all lines have been processed
rl.on('close',  ()=> {
    // console.log(`Lexing result: \n ${JSON.stringify(lexResult, null, 2)}`);
    const parseResult = parse(lexResult);
    // console.log(`Parsing result: \n ${JSON.stringify(parseResult, null, 2)}`);

    interpret(parseResult)
})
