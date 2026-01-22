var
where = 'client'
;
Package.describe({
name    : 'semantic:ui-css',
summary : 'Semantic UI - CSS Release of Semantic UI',
version : '2.2.10',
git     : 'git://github.com/Semantic-Org/Semantic-UI-CSS.git',
});
Package.onUse(function(api) {
api.versionsFrom('1.0');
api.use('jquery', 'client');
api.addFiles([
'themes/default/assets/fonts/icons.eot',
'themes/default/assets/fonts/icons.svg',
'themes/default/assets/fonts/icons.ttf',
'themes/default/assets/fonts/icons.woff',
'themes/default/assets/fonts/icons.woff2',
'themes/default/assets/images/flags.png',
'semantic.css',
'semantic.js'
], 'client');
});