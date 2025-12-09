# Advent of Code

Advent of code, in several languages for fun

(But mainly in Haskell)

## Use

### A new day :

Execute the `build_day.sh` file to generate the necessary files depending on the chosen language

You need to execute it like

```sh
. build_day.sh [hs]
```

### Profiling :

Build the Haskell Docker Image:

```sh
docker build -t haskell-aoc .

```

Create a haskell Docker
```sh
docker run -it --rm --name haskell-aoc haskell-aoc
```
Keep it running in the background in order to launch your solutions on it

#### Text based :

```sh
make profiling
```

which will run the profiling on the docker and create the result file as `DayXX.prof`

#### Visual Explorer

You can also use the flame graph viewer [SpeedScope](https://www.speedscope.app/)

In which case you will need to run this.

```sh
$ ghc -prof -fprof-auto -rtsopts Main.hs # replace with source file
$ ./Main +RTS -pj -RTS input.txt
```

You can run it locally, but if you have any issue, it should work on the Docker.

then you just need to do

```sh
docker cp haskell-aoc:/home/haskell/Main.prof .
```



## 2021

Started coding in Shell, was too difficult, so finished in Python..

For More [details](./2021/README.md)

## 2022

Starting in shell and Haskell, will see how it goes

For More [details](./2022/README.md)

## 2023

For More [details](./2023/README.md)

## 2024

Continuing in haskell

Having fun trying to automate stuff as well

For More [details](./2024/README.md)

### 2025

Still in haskell, trying to explain my thoughts and clean the code as much as possible by using the language's design

For More [details](./2025/README.md)
