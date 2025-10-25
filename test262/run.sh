# Clone repository at latest
if [ ! -d test262 ]
then
    git clone https://github.com/tc39/test262.git
else
    cd test262
    git pull
fi

# Run tests
cargo run --bin test262
