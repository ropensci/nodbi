#
# https://cran.r-project.org/web/packages/V8/vignettes/npm.html
#
# git clone https://github.com/gordonBusyman/mongo-to-sql-converter
# cd mongo-to-sql-converter
# ...
# git diff > ../update.patch
# rm -rf mongo-to-sql-converter
#
# Ivan Tarbakou, MIT License
module="mongo-to-sql-converter"
cd inst/js
npm install $module
patch -p1 -d node_modules/mongo-to-sql-converter <update.patch
echo "global.injs = require('$module');" >in.js
browserify in.js -o bundle.js
minify -o bundle.js bundle.js
rm -rf node_modules
rm package-lock.json
rm package.json
rm in.js
