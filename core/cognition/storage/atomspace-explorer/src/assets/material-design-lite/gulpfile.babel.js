'use strict';
import fs from 'fs';
import path from 'path';
import mergeStream from 'merge-stream';
import del from 'del';
import vinylPaths from 'vinyl-paths';
import runSequence from 'run-sequence';
import browserSync from 'browser-sync';
import through from 'through2';
import swig from 'swig';
import gulp from 'gulp';
import closureCompiler from 'gulp-closure-compiler';
import gulpLoadPlugins from 'gulp-load-plugins';
import uniffe from './utils/uniffe.js';
import pkg from './package.json';
const $ = gulpLoadPlugins();
const reload = browserSync.reload;
const hostedLibsUrlPrefix = 'https://code.getmdl.io';
const templateArchivePrefix = 'mdl-template-';
const bucketProd = 'gs://www.getmdl.io';
const bucketStaging = 'gs://mdl-staging';
const bucketCode = 'gs://code.getmdl.io';
const banner = ['',
''].join('\n');
let codeFiles = '';
const AUTOPREFIXER_BROWSERS = [
'ie >= 10',
'ie_mob >= 10',
'ff >= 30',
'chrome >= 34',
'safari >= 7',
'opera >= 23',
'ios >= 7',
'android >= 4.4',
'bb >= 10'
];
const SOURCES = [
'src/mdlComponentHandler.js',
'src/third_party*.js',
'src/button/button.js',
'src/checkbox/checkbox.js',
'src/icon-toggle/icon-toggle.js',
'src/menu/menu.js',
'src/progress/progress.js',
'src/radio/radio.js',
'src/slider/slider.js',
'src/snackbar/snackbar.js',
'src/spinner/spinner.js',
'src/switch/switch.js',
'src/tabs/tabs.js',
'src/textfield/textfield.js',
'src/tooltip/tooltip.js',
'src/layout/layout.js',
'src/data-table/data-table.js',
'src/ripple/ripple.js'
];
gulp.task('lint', () => {
return gulp.src([
'src*.js',
'gulpfile.babel.js'
])
.pipe(reload({stream: true, once: true}))
.pipe($.jshint())
.pipe($.jscs())
.pipe($.jshint.reporter('jshint-stylish'))
.pipe($.jscs.reporter())
.pipe($.if(!browserSync.active, $.jshint.reporter('fail')))
.pipe($.if(!browserSync.active, $.jscs.reporter('fail')));
});
gulp.task('images', () => {
return gulp.src('src*.{svg,png,jpg}')
.pipe($.flatten())
.pipe($.cache($.imagemin({
progressive: true,
interlaced: true
})))
.pipe(gulp.dest('dist/images'))
.pipe($.size({title: 'images'}));
});
gulp.task('styles:dev', () => {
return gulp.src('src*.scss')
.pipe($.sass({
precision: 10,
onError: console.error.bind(console, 'Sass error:')
}))
.pipe($.cssInlineImages({
webRoot: 'src'
}))
.pipe($.autoprefixer(AUTOPREFIXER_BROWSERS))
.pipe(gulp.dest('.tmp/styles'))
.pipe($.size({title: 'styles'}));
});
gulp.task('styletemplates', () => {
return gulp.src('src/template.scss')
.pipe($.sourcemaps.init())
.pipe($.sass({
precision: 10,
onError: console.error.bind(console, 'Sass error:')
}))
.pipe($.cssInlineImages({webRoot: 'src'}))
.pipe($.autoprefixer(AUTOPREFIXER_BROWSERS))
.pipe(gulp.dest('.tmp'))
.pipe($.concat('material.css.template'))
.pipe(gulp.dest('dist'))
.pipe($.if('*.css.template', $.csso()))
.pipe($.concat('material.min.css.template'))
.pipe($.header(banner, {pkg}))
.pipe($.sourcemaps.write('.'))
.pipe(gulp.dest('dist'))
.pipe($.size({title: 'styles'}));
});
gulp.task('styles', () => {
return gulp.src('src/material-design-lite.scss')
.pipe($.sourcemaps.init())
.pipe($.sass({
precision: 10,
onError: console.error.bind(console, 'Sass error:')
}))
.pipe($.cssInlineImages({webRoot: 'src'}))
.pipe($.autoprefixer(AUTOPREFIXER_BROWSERS))
.pipe(gulp.dest('.tmp'))
.pipe($.concat('material.css'))
.pipe($.header(banner, {pkg}))
.pipe(gulp.dest('dist'))
.pipe($.if('*.css', $.csso()))
.pipe($.concat('material.min.css'))
.pipe($.header(banner, {pkg}))
.pipe($.sourcemaps.write('.'))
.pipe(gulp.dest('dist'))
.pipe($.size({title: 'styles'}));
});
gulp.task('styles-grid', () => {
return gulp.src('src/material-design-lite-grid.scss')
.pipe($.sass({
precision: 10,
onError: console.error.bind(console, 'Sass error:')
}))
.pipe($.autoprefixer(AUTOPREFIXER_BROWSERS))
.pipe(gulp.dest('.tmp'))
.pipe($.concat('material-grid.css'))
.pipe($.header(banner, {pkg}))
.pipe(gulp.dest('dist'))
.pipe($.if('*.css', $.csso()))
.pipe($.concat('material-grid.min.css'))
.pipe($.header(banner, {pkg}))
.pipe(gulp.dest('dist'))
.pipe($.size({title: 'styles-grid'}));
});
gulp.task('closure', () => {
return gulp.src(SOURCES)
.pipe(closureCompiler({
compilerPath: 'node_modules/google-closure-compiler/compiler.jar',
fileName: 'material.closure.min.js',
compilerFlags: {
compilation_level: 'ADVANCED_OPTIMIZATIONS',
language_in: 'ECMASCRIPT6_STRICT',
language_out: 'ECMASCRIPT5_STRICT',
warning_level: 'VERBOSE'
}
}))
.pipe(gulp.dest('./dist'));
});
gulp.task('scripts', ['lint'], () => {
return gulp.src(SOURCES)
.pipe($.if(/mdlComponentHandler\.js/, $.util.noop(), uniffe()))
.pipe($.sourcemaps.init())
.pipe($.concat('material.js'))
.pipe($.iife({useStrict: true}))
.pipe(gulp.dest('dist'))
.pipe($.uglify({
sourceRoot: '.',
sourceMapIncludeSources: true
}))
.pipe($.header(banner, {pkg}))
.pipe($.concat('material.min.js'))
.pipe($.sourcemaps.write('.'))
.pipe(gulp.dest('dist'))
.pipe($.size({title: 'scripts'}));
});
gulp.task('clean', () => del(['dist', '.publish']));
gulp.task('metadata', () => {
return gulp.src([
'package.json',
'bower.json',
'LICENSE'
])
.pipe(gulp.dest('dist'));
});
gulp.task('default', ['clean'], cb => {
runSequence(
['styles', 'styles-grid'],
['scripts'],
['mocha'],
cb);
});
gulp.task('all', ['clean'], cb => {
runSequence(
['styletemplates'],
['styles-grid', 'styles:gen'],
['scripts'],
['mocha'],
['assets', 'pages',
'templates', 'images', 'metadata'],
['zip'],
cb);
});
gulp.task('mocha', ['styles'], () => {
return gulp.src('test/index.html')
.pipe($.mochaPhantomjs({reporter: 'tap'}));
});
gulp.task('mocha:closure', ['closure'], () => {
return gulp.src('test/index.html')
.pipe($.replace('src="../dist/material.js"',
'src="../dist/material.closure.min.js"'))
.pipe($.rename('temp.html'))
.pipe(gulp.dest('test'))
.pipe($.mochaPhantomjs({reporter: 'tap'}))
.on('finish', () => del.sync('test/temp.html'))
.on('error', () => del.sync('test/temp.html'));
});
gulp.task('test', [
'lint',
'mocha',
'mocha:closure'
]);
gulp.task('test:visual', () => {
browserSync({
notify: false,
server: '.',
startPath: 'test/visual/index.html'
});
gulp.watch('test/visual
const site = {};
function applyTemplate() {
return through.obj((file, enc, cb) => {
const data = {
site,
page: file.page,
content: file.contents.toString()
};
const templateFile = path.join(
__dirname, 'docs', '_templates', `${file.page.layout}.html`);
const tpl = swig.compileFile(templateFile, {cache: false});
file.contents = new Buffer(tpl(data));
cb(null, file);
});
}
gulp.task('components', ['demos'], () => {
return gulp.src('srcREADME.md', {base: 'src'})
.pipe($.header('---\nlayout: component\nbodyclass: component\ninclude_prefix: ../../\n---\n\n'))
.pipe($.frontMatter({
property: 'page',
remove: true
}))
.pipe($.marked())
.pipe((() => {
return through.obj((file, enc, cb) => {
file.page.component = file.relative.split('/')[0];
cb(null, file);
});
})())
.pipe(applyTemplate())
.pipe($.rename(path => path.basename = 'index'))
.pipe(gulp.dest('dist/components'));
});
gulp.task('demoresources', () => {
return gulp.src([
'srcdemos.css',
'srcdemo.css',
'srcdemo.js'
], {base: 'src'})
.pipe($.if('*.scss', $.sass({
precision: 10,
onError: console.error.bind(console, 'Sass error:')
})))
.pipe($.cssInlineImages({webRoot: 'src'}))
.pipe($.if('*.css', $.autoprefixer(AUTOPREFIXER_BROWSERS)))
.pipe(gulp.dest('dist/components'));
});
gulp.task('demos', ['demoresources'], () => {
function getComponentFolders() {
return fs.readdirSync('src')
.filter(file => fs.statSync(path.join('src', file)).isDirectory());
}
const tasks = getComponentFolders().map(component => {
return gulp.src([
path.join('src', component, 'snippets', '*.html'),
path.join('src', component, 'demo.html')
])
.pipe($.concat('/demo.html'))
.pipe($.header('---\nlayout: demo\nbodyclass: demo\ninclude_prefix: ../../\n---\n\n'))
.pipe($.frontMatter({
property: 'page',
remove: true
}))
.pipe($.marked())
.pipe((() => {
return through.obj((file, enc, cb) => {
file.page.component = component;
cb(null, file);
});
})())
.pipe(applyTemplate())
.pipe(gulp.dest(path.join('dist', 'components', component)));
});
return mergeStream(tasks);
});
gulp.task('pages', ['components'], () => {
return gulp.src('docs/_pages
.pipe($.replace('class="lang-', 'class="language-'))
.pipe($.replace('class="language-html', 'class="language-markup'))
.pipe($.rename(path => {
if (path.basename !== 'index') {
path.dirname = path.basename;
path.basename = 'index';
}
}))
.pipe(gulp.dest('dist'));
});
gulp.task('assets', () => {
return gulp.src([
'docs/_assets*',
'node_modules/clippy/build/clippy.swf',
'node_modules/swfobject-npm/swfobject/src/swfobject.js',
'node_modules/prismjs/prism.js',
'node_modules/prismjs/components/prism-markup.min.js',
'node_modules/prismjs/components/prism-javascript.min.js',
'node_modules/prismjs/components/prism-css.min.js',
'node_modules/prismjs/components/prism-bash.min.js',
'node_modules/prismjs/dist/prism-default/prism-default.css'
])
.pipe($.if(/\.js/i, $.replace('$$version$$', pkg.version)))
.pipe($.if(/\.js/i, $.replace('$$hosted_libs_prefix$$', hostedLibsUrlPrefix)))
.pipe($.if(/\.(svg|jpg|png)$/i, $.imagemin({
progressive: true,
interlaced: true
})))
.pipe($.if(/\.css/i, $.autoprefixer(AUTOPREFIXER_BROWSERS)))
.pipe($.if(/\.css/i, $.csso()))
.pipe($.if(/\.js/i, $.uglify({
preserveComments: 'some',
sourceRoot: '.',
sourceMapIncludeSources: true
})))
.pipe(gulp.dest('dist/assets'));
});
function watch() {
gulp.watch(['src*.js', '!srcREADME.md'],
['scripts', 'demos', 'components', reload]);
gulp.watch(['src*.{scss,css}'],
['styles', 'styles-grid', 'styletemplates', reload]);
gulp.watch(['src*.html'], ['pages', reload]);
gulp.watch(['src*.{svg,png,jpg}'], ['images', reload]);
gulp.watch(['srcREADME.md'], ['pages', reload]);
gulp.watch(['templates*'], ['templates', reload]);
gulp.watch(['docs*'], ['pages', 'assets', reload]);
gulp.watch(['package.json', 'bower.json', 'LICENSE'], ['metadata']);
}
gulp.task('serve:browsersync', () => {
browserSync({
notify: false,
server: {
baseDir: ['dist']
}
});
watch();
});
gulp.task('serve', () => {
$.connect.server({
root: 'dist',
port: 5000,
livereload: true
});
watch();
gulp.src('dist/index.html')
.pipe($.open({uri: 'http://localhost:5000'}));
});
gulp.task('zip:mdl', () => {
return gulp.src([
'dist/material?(.min)@(.js|.css)?(.map)',
'LICENSE',
'bower.json',
'package.json'
])
.pipe($.zip('mdl.zip'))
.pipe(gulp.dest('dist'));
});
function getSubDirectories(dir) {
return fs.readdirSync(dir)
.filter(file => fs.statSync(path.join(dir, file)).isDirectory());
}
gulp.task('zip:templates', () => {
const templates = getSubDirectories('dist/templates');
const generateZips = templates.map(template => {
return gulp.src([
`dist/templates/${template}*.*`,
'LICENSE'
])
.pipe($.rename(path => {
path.dirname = path.dirname.replace(`dist/templates/${template}`, '');
}))
.pipe($.zip(`${templateArchivePrefix}${template}.zip`))
.pipe(gulp.dest('dist'));
});
return mergeStream(generateZips);
});
gulp.task('zip', [
'zip:templates',
'zip:mdl'
]);
gulp.task('genCodeFiles', () => {
return gulp.src([
'dist/material.*@(js|css)?(.map)',
'dist/mdl.zip',
`dist/${templateArchivePrefix}*.zip`
], {read: false})
.pipe($.tap(file => {
codeFiles += ` dist/${path.basename(file.path)}`;
}));
});
gulp.task('pushCodeFiles', () => {
const dest = bucketCode;
console.log(`Publishing ${pkg.version} to CDN (${dest})`);
const cacheControl = '-h "Cache-Control:public,max-age=2592000"';
const gsutilCpCmd = 'gsutil -m cp -z js,css,map ';
const gsutilCacheCmd = `gsutil -m setmeta -R ${cacheControl}`;
return gulp.src('')
.pipe($.shell([
`${gsutilCpCmd}${codeFiles} ${dest}/${pkg.version}`,
`${gsutilCacheCmd} ${dest}/${pkg.version}`
]));
});
gulp.task('publish:code', cb => {
runSequence(
['zip:mdl', 'zip:templates'],
'genCodeFiles',
'pushCodeFiles',
cb);
});
function mdlPublish(pubScope) {
let cacheTtl = null;
let src = null;
let dest = null;
if (pubScope === 'staging') {
cacheTtl = 0;
src = 'dist*',
'gulpfile.babel.js',
'./util?*'
])
.pipe(gulp.dest('_release'));
});
gulp.task('publish:release', ['_release'], () => {
return gulp.src('_release')
.pipe($.subtree({
remote: 'origin',
branch: 'release'
}))
.pipe(vinylPaths(del));
});
gulp.task('templates:styles', () => {
return gulp.src('templates*.css')
.pipe($.autoprefixer(AUTOPREFIXER_BROWSERS))
.pipe(gulp.dest('dist/templates'));
});
gulp.task('templates:static', () => {
return gulp.src('templates*.html')
.pipe($.replace('$$version$$', pkg.version))
.pipe($.replace('$$hosted_libs_prefix$$', hostedLibsUrlPrefix))
.pipe(gulp.dest('dist/templates'));
});
gulp.task('templates:localtestingoverride', () => {
return gulp.src('templates*.html')
.pipe($.replace('$$version$$', '.'))
.pipe($.replace('$$hosted_libs_prefix$$', ''))
.pipe(gulp.dest('dist/templates'));
});
gulp.task('templates:images', () => {
return gulp.src('templates*')
.pipe($.imagemin({
progressive: true,
interlaced: true
}))
.pipe(gulp.dest('dist/templates'));
});
gulp.task('templates:fonts', () => {
return gulp.src('templates*')
.pipe(gulp.dest('dist/templates/'));
});
gulp.task('templates', [
'templates:static',
'templates:images',
'templates:fonts',
'templates:styles'
]);
gulp.task('styles:gen', ['styles'], () => {
const MaterialCustomizer = require('./docs/_assets/customizer.js');
const templatePath = path.join(__dirname, 'dist', 'material.min.css.template');
const mc = new MaterialCustomizer();
mc.template = fs.readFileSync(templatePath).toString();
let stream = gulp.src('');
mc.paletteIndices.forEach(primary => {
mc.paletteIndices.forEach(accent => {
if (primary === accent) {
return;
}
if (mc.forbiddenAccents.indexOf(accent) !== -1) {
return;
}
const primaryName = primary.toLowerCase().replace(' ', '_');
const accentName = accent.toLowerCase().replace(' ', '_');
stream = stream.pipe($.file(
`material.${primaryName}-${accentName}.min.css`,
mc.processTemplate(primary, accent)
));
});
});
stream.pipe(gulp.dest('dist'));
});