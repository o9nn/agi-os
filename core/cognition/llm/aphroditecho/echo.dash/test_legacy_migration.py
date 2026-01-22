from pathlib import Path
def test_legacy_files_archived():
    print('🧪 Testing legacy file migration...')
    repo_root = Path(__file__).parent
    archive_legacy = repo_root / 'archive' / 'legacy'
    legacy_files = ['deep_tree_echo-v1.py', 'deep_tree_echo-v2.py']
    for file in legacy_files:
        root_file = repo_root / file
        assert not root_file.exists(), f'Legacy file {file} should not exist in root directory'
        print(f'  ✅ {file} correctly removed from root')
    for file in legacy_files:
        archived_file = archive_legacy / file
        assert archived_file.exists(), f'Legacy file {file} should exist in archive/legacy/'
        print(f'  ✅ {file} correctly archived in archive/legacy/')
    print('  ✅ All legacy file migration tests passed')
def test_archive_structure():
    print('🧪 Testing archive directory structure...')
    repo_root = Path(__file__).parent
    archive_dir = repo_root / 'archive'
    legacy_dir = archive_dir / 'legacy'
    assert archive_dir.exists(), 'Archive directory should exist'
    assert legacy_dir.exists(), 'Archive/legacy directory should exist'
    readme = archive_dir / 'README.md'
    assert readme.exists(), 'Archive README.md should exist'
    print('  ✅ Archive structure tests passed')
def test_no_legacy_references_in_analyzer():
    print('🧪 Testing analyzer gap detection...')
    repo_root = Path(__file__).parent
    import sys
    sys.path.insert(0, str(repo_root))
    from deep_tree_echo_analyzer import DeepTreeEchoAnalyzer
    analyzer = DeepTreeEchoAnalyzer(repo_root)
    gaps = analyzer.identify_architecture_gaps()
    legacy_gaps = [gap for gap in gaps if gap['gap'] == 'Legacy Code Retention']
    assert len(legacy_gaps) == 0, 'Legacy Code Retention gap should not be detected'
    print('  ✅ Analyzer correctly does not detect legacy code retention gap')
def run_all_tests():
    print('🚀 Starting Legacy Migration Validation Tests')
    print('=' * 50)
    try:
        test_legacy_files_archived()
        test_archive_structure()
        test_no_legacy_references_in_analyzer()
        print('\n' + '=' * 50)
        print('✅ All legacy migration tests passed!')
        print('\n🎯 Migration validation successful:')
        print('  - Legacy files properly archived in archive/legacy/')
        print('  - Root directory cleaned of deprecated versions')
        print('  - Archive structure correctly organized')
        print('  - Analyzer no longer detects legacy code retention gap')
        return True
    except Exception as e:
        print(f'\n❌ Test failed: {e}')
        import traceback
        traceback.print_exc()
        return False
if __name__ == '__main__':
    success = run_all_tests()
    import sys
    sys.exit(0 if success else 1)