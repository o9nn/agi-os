# Development Workflows - Extended Definition

## Development Environment Setup

```bash
# Install dependencies
sudo apt-get install build-essential cmake git libboost-all-dev

# Clone repository
git clone https://github.com/o9nn/agi-os.git
cd agi-os

# Build
./build-agi-os.sh
```

## Code Style

- **C/C++**: GNU style for microkernel/OS, OpenCog style for cognition
- **Python**: PEP 8
- **Scheme**: Guile conventions
- **Documentation**: Markdown

## Testing

```bash
# Unit tests
cd component/build
make test

# Integration tests
cd infrastructure/testing/integration
./run-tests.sh
```

## Contributing

1. Fork repository
2. Create feature branch
3. Make changes
4. Run tests
5. Submit pull request

## References

- Contributing Guide: `CONTRIBUTING.md`
- Code Style: `documentation/guides/code-style.md`
