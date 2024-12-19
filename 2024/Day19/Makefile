CONTAINER_NAME=haskell-aoc
SRC=./Day19.hs
TARGET=Day19
all: $(TARGET)

$(TARGET): $(SRC)
	ghc -O3 $(SRC)

profile: $(SRC)


clean:
	$(RM) ./Day19 ./Day19.o ./Day19.hi

setup: $(SRC)
	docker cp $(SRC) $(CONTAINER_NAME):/home/haskell/Main.hs
	docker cp input.txt $(CONTAINER_NAME):/home/haskell/input.txt
	docker cp shortinput.txt $(CONTAINER_NAME):/home/haskell/shortinput.txt

profiling: setup
	docker exec -it $(CONTAINER_NAME) cabal build --enable-profiling
	docker exec -it $(CONTAINER_NAME) ./profile input.txt

run: setup
	docker exec -it $(CONTAINER_NAME) cabal build
	docker exec -it $(CONTAINER_NAME) ./run input.txt

.PHONY: all $(TARGET) clean setup profiling run
