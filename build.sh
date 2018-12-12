elm make app/elm/Main.elm --optimize --output=public/js/main.js
cd public/js && uglifyjs main.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=main.min.js
rm main.js && mv main.min.js main.js