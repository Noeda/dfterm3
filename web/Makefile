SRC := main.js connection.js login_ui.js
MAIN := main.js

debug: $(SRC)
	browserify main.js -o bundle.js

release: $(SRC)
	browserify main.js -o tmp.js
	uglifyjs tmp.js > bundle.js
	rm tmp.js

# TODO: write something that also minimifies bundle.js (closure compiler or
# uglify.js maybe?)
#
