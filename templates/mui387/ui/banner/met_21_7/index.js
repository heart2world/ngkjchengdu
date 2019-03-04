METUI_FUN['$uicss'] = {
    name: '$uicss',
    bannerInit: function() {
        if (METUI['$uicss'].length) {
            var item = $('.$uicss .banner-item'),
                ctrl = $('.$uicss .banner-ctrl'),
                prev = $('.$uicss .banner-ctrl .left'),
                next = $('.$uicss .banner-ctrl .right'),
                ny = $('.banner-ny-h');
            if (item.length < 2) {
                ctrl.addClass('hide');
            }
            if (item.length > 0) {
                bh = item.data('height').split('|'),
                    autoplayspeed = item.data('autoplayspeed'),
                    speed = item.data('speed');

                for (var i = 0; i < bh.length; i++) {
                    if (bh[i] == 0) {
                        bh[i] = '100%';
                    }
                }
                Breakpoints.on('md lg', {
                    enter: function() {
                        ny.height(bh[0]);
                        item.height(bh[0]);
                    }
                })
                Breakpoints.on('sm', {
                    enter: function() {
                        ny.height(bh[1]);
                        item.height(bh[1]);
                    }
                })
                Breakpoints.on('xs', {
                    enter: function() {
                        ny.height(bh[2]);
                        item.height(bh[2]);
                    }
                })
                if (item.length > 1) {
                    var text = $('.banner-text');
                    $('.banner').addClass('isSwiper')
                    M['banner'] = new Swiper('.banner', {
                        wrapperClass: 'banner-warpper',
                        slideClass: 'banner-item',
                        speed: speed,
                        loop: true,
                        autoplay: autoplayspeed,
                        autoplayDisableOnInteraction: true,
                        keyboardControl: true,
                        slidesPerView: 1,
                        simulateTouch: false,
                        paginationClickable: true,
                        onInit: function(swiper) {
                            swiperAnimateCache(swiper);
                            swiperAnimate(swiper);
                        },
                        onSlideChangeEnd: function(swiper) {
                            swiperAnimate(swiper);
                        }
                    });
                    prev.click(function() {
                        M['banner'].slidePrev();
                    });
                    next.click(function() {
                        M['banner'].slideNext();
                    });
                }
            }
        }
    },
    video: function() {

        var v = $('.met-video'),
            datav = $('.metvideobox').attr("data-metvideo"),
            m = $('#media'),
            w = $(window).width(),
            h = $(window).height();
            console.log(h);
        if ($('.metvideobox').length > 0) {
            data = datav.split("|"),
                src = data[4];
            m.attr("src", src);
            setTimeout(function(){
                m.trigger('play');
            },1000)
            // $('.banner-ctrl').addClass('hide');
            bgResize(m, w, h, 1920, 1080);
            $(window).resize(function() {
                var video = $('#media'),
                    w = $(window).width(),
                    h = $(window).height();
                bgResize(video, w, h, 1920, 1080);
            })

        }
    }
};
var banner = metui(METUI_FUN['$uicss']);

function bgResize(img, width, height, bg_width, bg_height) {
    var w = width,
        h = width / bg_width * bg_height;
    if (h < height) {
        h = height;
        w = height / bg_height * bg_width;
    }
    img.css({
        height: h,
        width: w,
        marginTop: -(h - height) / 2,
        marginLeft: -(w - width) / 2,
        'visibility': 'visible'
    });
}

(function(){
    $.fn.extend({
        pullpage_banner_fun:function(num){
            $(".$uicss video").trigger('play');
        }

      });
})(jQuery);