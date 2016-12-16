all: day5

day%: FORCE
	@(cd $@ && cargo clean && cargo build -q --release && ./target/release/$@)

FORCE:
