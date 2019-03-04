METUI_FUN['$uicss'] = {
    name:'$uicss',
    appear:function (){
        // 首页首屏内动画预加载
        var indexappear=$('.met-index-body:eq(0) [data-plugin=appear]');
        if(indexappear.length){
            indexappear.scrollFun(function(val){
                val.appearDiy();
            });
        }

        var Expand = function () {
            var tile = $('.strips__strip');
            var tileLink = $('.strips__strip > .strip__content');
            var tileText = tileLink.find('.strip__inner-text');
            var stripClose = $('.strip__close');
            var expanded = false;
            if($(window).width() < 768){
                $(".$uicss .strips__strip").each(function(index, el) {
                    var modh = $(".$uicss .strips__strip").eq(index).attr("data-modh");
                    $(".$uicss .strips__strip").eq(index).css("height",modh+"vh");

                });
            }
            var open = function () {
                var tile = $(this).parent();
                if (!expanded) {
                    if($(window).width() < 768){
                        tile.css('top',"-"+(tile.data("index") * tile.data("modh"))+"vh");
                    }
                    tile.addClass('strips__strip--expanded');
                    tileText.css('transition', 'all .6s');
                    stripClose.addClass('strip__close--show');
                    stripClose.css('transition', 'all .6s');
                    expanded = true;
                }
            };
            var close = function () {
                if (expanded) {
                    if($(window).width() < 768){
                        tile.css('top',"0");
                    }
                    tile.removeClass('strips__strip--expanded');
                    tileText.css('transition', 'all .5s');
                    stripClose.removeClass('strip__close--show');
                    stripClose.css('transition', 'all .5s');
                    expanded = false;
                }
            };
            var bindActions = function () {
                tileLink.on('click', open);
                stripClose.on('click', close);
            };
            var init = function () {
                bindActions();
            };
            return { init: init };
        }();
        Expand.init();
    },
    getRGB: function() {
            function hex(){
                /*背景颜色透明*/
                var t = METUI['$uicss'].find('.strip__inner-text'),
                    b = t.data('hex');
                if (t.length) {
                    var hex = b.split('|')[0],
                        opacity = b.split('|')[1];
                    var bgcolor = rgb2color(hex, opacity);
                    t.css('background', bgcolor);
                }
                function rgb2color(hex, opacity) {
                    var reg = /^#([0-9a-fA-f]{3}|[0-9a-fA-f]{6})$/;
                    var c = hex.toLowerCase();
                    if (c && reg.test(c)) {
                        if (c.length === 4) {
                            var a = '#';
                            for (var i = 1; i < 4; i++) {
                                a += c.slice(i, i + 1).concat(c.slice(i, i + 1));
                            }
                            c = a;
                        }
                        var b = [];
                        for (var i = 1; i < 7; i += 2) {
                            b.push(parseInt('0x' + c.slice(i, i + 2)));
                        }
                        return "rgba(" + b.join(',') + ',' + opacity + ')';
                    } else {
                        return c
                    }

                }
            }
            hex();
        }
};
var x = new metui(METUI_FUN['$uicss']);
