#!/usr/bin/env node

//1. read file from disc
const fs = require('fs');
const readline = require('readline');
const {lex} = require("./lexer");

const filePath = 'demo-code.txt';

// Create a readable stream from the file
const fileStream = fs.createReadStream(filePath);

// Create a readline interface
const rl = readline.createInterface({
    input: fileStream,
    crlfDelay: Infinity // Recognize all instances of CR LF ('\r\n') as a single line break
});

// Event handler for each line
rl.on('line', (line) => {
    console.log(`Line from file: ${line}`);
    console.log(`Lexing result: \n ${JSON.stringify(lex(line), null, 2)}`);
});

// Event handler for the end of the file
rl.on('close', () => {
    console.log('File reading finished.');
});

