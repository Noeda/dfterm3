// Implements a "terminal emulator". that is in quotes because the only thing
// it emulates is the appearance of a terminal emulator and otherwise has
// nothing to do with terminals.
//

dfterm3_terminal = function() {
    var exports = { };

    var cp437_table = { };
    cp437_table[0] = 0x00a0;
    cp437_table[1] = 0x263a;
    cp437_table[2] = 0x263b;
    cp437_table[3] = 0x2665;
    cp437_table[4] = 0x2666;
    cp437_table[5] = 0x2663;
    cp437_table[6] = 0x2660;
    cp437_table[7] = 0x2022;
    cp437_table[8] = 0x25d8;
    cp437_table[9] = 0x25cb;
    cp437_table[10] = 0x25d9;
    cp437_table[11] = 0x2642;
    cp437_table[12] = 0x2640;
    cp437_table[13] = 0x266a;
    cp437_table[14] = 0x266b;
    cp437_table[15] = 0x263c;
    cp437_table[16] = 0x25ba;
    cp437_table[17] = 0x25c4;
    cp437_table[18] = 0x2195;
    cp437_table[19] = 0x203c;
    cp437_table[20] = 0x00b6;
    cp437_table[21] = 0x00a7;
    cp437_table[22] = 0x25ac;
    cp437_table[23] = 0x21a8;
    cp437_table[24] = 0x2191;
    cp437_table[25] = 0x2193;
    cp437_table[26] = 0x2192;
    cp437_table[27] = 0x2190;
    cp437_table[28] = 0x221f;
    cp437_table[29] = 0x2194;
    cp437_table[30] = 0x25b2;
    cp437_table[31] = 0x25bc;
    cp437_table[32] = 0x00a0;   // non-breaking space prevents spans from
                                // becoming empty and messing up the terminal
    // values between 32-127 are the same as ASCII
    cp437_table[127] = 0xfffd;
    cp437_table[128] = 0x00c7;
    cp437_table[129] = 0x00fc;
    cp437_table[130] = 0x00e9;
    cp437_table[131] = 0x00e2;
    cp437_table[132] = 0x00e4;
    cp437_table[133] = 0x00e0;
    cp437_table[134] = 0x00e5;
    cp437_table[135] = 0x00e7;
    cp437_table[136] = 0x00ea;
    cp437_table[137] = 0x00eb;
    cp437_table[138] = 0x00e8;
    cp437_table[139] = 0x00ef;
    cp437_table[140] = 0x00ee;
    cp437_table[141] = 0x00ec;
    cp437_table[142] = 0x00c4;
    cp437_table[143] = 0x00e5;
    cp437_table[144] = 0x00c9;
    cp437_table[145] = 0x00e6;
    cp437_table[146] = 0x00c6;
    cp437_table[147] = 0x00f4;
    cp437_table[148] = 0x00f6;
    cp437_table[149] = 0x00f2;
    cp437_table[150] = 0x00fb;
    cp437_table[151] = 0x00f9;
    cp437_table[152] = 0x00ff;
    cp437_table[153] = 0x00d6;
    cp437_table[154] = 0x00dc;
    cp437_table[155] = 0x00a2;
    cp437_table[156] = 0x00a3;
    cp437_table[157] = 0x00a5;
    cp437_table[158] = 0x20a7;
    cp437_table[159] = 0x0192;
    cp437_table[160] = 0x00e1;
    cp437_table[161] = 0x00ed;
    cp437_table[162] = 0x00f3;
    cp437_table[163] = 0x00fa;
    cp437_table[164] = 0x00f1;
    cp437_table[165] = 0x00d1;
    cp437_table[166] = 0x00aa;
    cp437_table[167] = 0x00ba;
    cp437_table[168] = 0x00bf;
    cp437_table[169] = 0x2310;
    cp437_table[170] = 0x00ac;
    cp437_table[171] = 0x00bd;
    cp437_table[172] = 0x00bc;
    cp437_table[173] = 0x00a1;
    cp437_table[174] = 0x00ab;
    cp437_table[175] = 0x00bb;
    cp437_table[176] = 0x2591;
    cp437_table[177] = 0x2592;
    cp437_table[178] = 0x2593;
    cp437_table[179] = 0x2502;
    cp437_table[180] = 0x2524;
    cp437_table[181] = 0x2561;
    cp437_table[182] = 0x2562;
    cp437_table[183] = 0x2556;
    cp437_table[184] = 0x2555;
    cp437_table[185] = 0x2563;
    cp437_table[186] = 0x2551;
    cp437_table[187] = 0x2557;
    cp437_table[188] = 0x255d;
    cp437_table[189] = 0x255c;
    cp437_table[190] = 0x255b;
    cp437_table[191] = 0x2510;
    cp437_table[192] = 0x2514;
    cp437_table[193] = 0x2534;
    cp437_table[194] = 0x252c;
    cp437_table[195] = 0x251c;
    cp437_table[196] = 0x2500;
    cp437_table[197] = 0x253c;
    cp437_table[198] = 0x255e;
    cp437_table[199] = 0x255f;
    cp437_table[200] = 0x255a;
    cp437_table[201] = 0x2554;
    cp437_table[202] = 0x2569;
    cp437_table[203] = 0x2566;
    cp437_table[204] = 0x2560;
    cp437_table[205] = 0x2550;
    cp437_table[206] = 0x256c;
    cp437_table[207] = 0x2567;
    cp437_table[208] = 0x2568;
    cp437_table[209] = 0x2564;
    cp437_table[210] = 0x2565;
    cp437_table[211] = 0x2559;
    cp437_table[212] = 0x2558;
    cp437_table[213] = 0x2552;
    cp437_table[214] = 0x2553;
    cp437_table[215] = 0x256b;
    cp437_table[216] = 0x256a;
    cp437_table[217] = 0x2518;
    cp437_table[218] = 0x250c;
    cp437_table[219] = 0x2588;
    cp437_table[220] = 0x2584;
    cp437_table[221] = 0x258c;
    cp437_table[222] = 0x2590;
    cp437_table[223] = 0x2580;
    cp437_table[224] = 0x03b1;
    cp437_table[225] = 0x00df;
    cp437_table[226] = 0x0393;
    cp437_table[227] = 0x03c0;
    cp437_table[228] = 0x03a3;
    cp437_table[229] = 0x03c3;
    cp437_table[230] = 0x00b5;
    cp437_table[231] = 0x03c4;
    cp437_table[232] = 0x03a6;
    cp437_table[233] = 0x0398;
    cp437_table[234] = 0x03a9;
    cp437_table[235] = 0x03b4;
    cp437_table[236] = 0x221e;
    cp437_table[237] = 0x03c6;
    cp437_table[238] = 0x03b5;
    cp437_table[239] = 0x2229;
    cp437_table[240] = 0x2261;
    cp437_table[241] = 0x00b1;
    cp437_table[242] = 0x2265;
    cp437_table[243] = 0x2264;
    cp437_table[244] = 0x2320;
    cp437_table[245] = 0x2321;
    cp437_table[246] = 0x00F7;
    cp437_table[247] = 0x2248;
    cp437_table[248] = 0x00b0;
    cp437_table[249] = 0x2219;
    cp437_table[250] = 0x00b7;
    cp437_table[251] = 0x221a;
    cp437_table[252] = 0x207f;
    cp437_table[253] = 0x00b2;
    cp437_table[254] = 0x25a0;
    cp437_table[255] = 0x00a0;

    var mapper = function( cp437 ) {
        if ( cp437 <= 32 ) {
            return cp437_table[cp437];
        } else if ( cp437 < 127 ) {
            return cp437;
        } else {
            return cp437_table[cp437];
        }
    }

    var mapCp437ToUnicode = function ( cp437 ) {
        return String.fromCharCode(mapper(cp437));
    }

    exports.colorsToStyle = function( foreground_color, background_color ) {
        var fstyle = "";
        switch ( foreground_color ) {
            case 0: fstyle = "fblack "; break;
            case 1: fstyle = "fred "; break;
            case 2: fstyle = "fgreen "; break;
            case 3: fstyle = "fyellow "; break;
            case 4: fstyle = "fblue "; break;
            case 5: fstyle = "fmagenta "; break;
            case 6: fstyle = "fcyan "; break;
            case 7: fstyle = "fgray "; break;
            case 8: fstyle = "fdarkgray "; break;
            case 9: fstyle = "fbrightred "; break;
            case 10: fstyle = "fbrightgreen "; break;
            case 11: fstyle = "fbrightyellow "; break;
            case 12: fstyle = "fbrightblue "; break;
            case 13: fstyle = "fbrightmagenta "; break;
            case 14: fstyle = "fbrightcyan "; break;
            case 15: fstyle = "fwhite "; break;
        }
        switch ( background_color ) {
            case 0: bstyle = "bblack"; break;
            case 1: bstyle = "bred"; break;
            case 2: bstyle = "bgreen"; break;
            case 3: bstyle = "byellow"; break;
            case 4: bstyle = "bblue"; break;
            case 5: bstyle = "bmagenta"; break;
            case 6: bstyle = "bcyan"; break;
            case 7: bstyle = "bgray"; break;
            case 8: bstyle = "bdarkgray"; break;
            case 9: bstyle = "bbrightred"; break;
            case 10: bstyle = "bbrightgreen"; break;
            case 11: bstyle = "bbrightyellow"; break;
            case 12: bstyle = "bbrightblue"; break;
            case 13: bstyle = "bbrightmagenta"; break;
            case 14: bstyle = "bbrightcyan"; break;
            case 15: bstyle = "bwhite"; break;
        }
        return fstyle + bstyle;
    }

    // Creates a terminal. Returns an object that you can link to the document
    // by using `getTerminalObject()`.
    //
    // You need to specify initial width and height.
    exports.createTerminal = function( width, height ) {
        var result;     // returned from this function
        var span_map = { }

        result = document.createElement("div");
        result.setAttribute("class", "dfterm3_terminal");

        for ( var x = 0; x < width; ++x ) {
            span_map[x] = { }
        }

        for ( var y = 0; y < height; ++y ) {
            for ( var x = 0; x < width; ++x ) {
                var e = document.createElement("span");
                e.setAttribute("class", "fgray bblack");
                e.textContent = "A";
                result.appendChild(e);
                span_map[x][y] = e;
            }
            var br = document.createElement("br");
            result.appendChild(br);
        }

        function resize( new_w, new_h ) {
            if ( new_w == this.width() &&
                 new_h == this.height() ) {
                return;
            }
            while ( result.firstChild ) {
                result.removeChild(result.firstChild);
            }

            this.width = function() { return new_w; }
            this.height = function() { return new_h; }

            span_map = { };

            for ( var x = 0; x < new_w; ++x ) {
                span_map[x] = { }
            }

            for ( var y = 0; y < new_h; ++y ) {
                for ( var x = 0; x < new_w; ++x ) {
                    var e = document.createElement("span");
                    e.setAttribute("class", "fgray bblack");
                    e.textContent = "A";
                    result.appendChild(e);
                    span_map[x][y] = e;
                }
                var br = document.createElement("br");
                result.appendChild(br);
            }
        }

        return { getDOMObject : function () { return result; }
               , cellAt : function ( x, y ) { return span_map[x][y]; }
               , width : function() { return width; }
               , height : function() { return height; }
               , resize : resize
               , setCP437CellAt : function ( x
                                           , y
                                           , cp437
                                           , f
                                           , b ) {
                     var unicode = mapCp437ToUnicode(cp437);
                     var cell = this.cellAt( x, y );
                     cell.textContent = unicode;
                     cell.setAttribute("class", exports.colorsToStyle(f, b));
                 } }
    }

    return exports;
}();

