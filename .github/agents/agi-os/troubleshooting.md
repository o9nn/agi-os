# Troubleshooting Guide - Extended Definition

## Common Issues

### Build Failures

**Issue**: atomspace-storage not found when building cogserver

**Solution**: Ensure atomspace-storage is built before cogserver:
```bash
./build-agi-os.sh  # Use unified build script
```

### Runtime Issues

**Issue**: AtomSpace not accessible

**Solution**: Check storage backend:
```bash
# Check if storage backend is running
ps aux | grep rocksdb

# Restart storage backend
systemctl restart atomspace-storage
```

### Performance Issues

**Issue**: Slow AtomSpace access

**Solution**: Enable caching:
```bash
# In /etc/opencog/atomspace.conf
(define atomspace-config
  '((cache-enabled . #t)
    (cache-size . 100000)))
```

## Debug Tools

```bash
# Enable debug logging
export OPENCOG_LOG_LEVEL=DEBUG

# View AtomSpace stats
cat /proc/cognitive/atomspace

# View attention stats
cat /proc/cognitive/attention
```

## References

- Build System: `.github/agents/agi-os/build-system.md`
- Documentation: `documentation/`
