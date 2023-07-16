## Syntax-aware Text Editor

This is a syntax-aware text editor for the language defined in exercise a2. It is written in Haskell using GTK+ and the gi-gtk library.

## Installing Stack

To build and run this app, you need to have Stack installed on your system. Here are the instructions for installing Stack:

### Linux

Open your terminal and download and install Stack by running the following command:

bashCopy code

`curl -sSL https://get.haskellstack.org/ | sh` 

### macOS

You can install Stack on macOS with Homebrew:

bashCopy code

`brew install haskell-stack` 

### Windows

On Windows, download and install the [Stack setup exe](https://get.haskellstack.org/stable/windows-x86_64-installer.exe).

More detailed instructions can be found in the [Stack documentation](https://docs.haskellstack.org/en/stable/install_and_upgrade/).



After installing run the following command:

```bash
stack setup
```

This will install ghc and prepare your environment.



## Building the App

Once you have Stack installed, navigate to the root directory of this project (the directory that contains the `stack.yaml` file) and run the following command to build the app:

```bash
stack build
```

This will download and install the necessary dependencies and then compile the app.

## Running the App

To run the app, use the following command:

```bash
stack run
```

This will start the text editor.
