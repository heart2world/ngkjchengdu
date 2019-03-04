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
        if($(window).width() <= 767){
            var swiper = new Swiper('.$uicss .swiper-container', {
                slidesPerView: 1,
                speed:500,
                autoplay:false,
                spaceBetween:0,
                grabCursor: true,
                simulateTouch : true,
                centeredSlides: true,
                loop: true,
                touchRatio : 0.5,
                navigation: {
                    nextEl: '.button-next',
                    prevEl: '.button-prev',
                },
            });
        }else if($(window).width() <= 1024){
            if($(".$uicss .swiper-slide").length <= 1){
                METUI['$uicss'].find('.buttons').css("display","none");
            }
            var num_oks = $(".$uicss .swiper-slide").length <= 2?false:true;
            var swiper = new Swiper('.$uicss .swiper-container', {
                slidesPerView: 2,
                spaceBetween:30,
                autoplay:false,
                speed:500,
                grabCursor: true,
                simulateTouch : true,
                centeredSlides: true,
                loop: num_oks,
                touchRatio : 0.5,
                navigation: {
                    nextEl: '.button-next',
                    prevEl: '.button-prev',
                },
            }); 
        }else{
            if($(".$uicss .swiper-slide").length <= 1){
                METUI['$uicss'].find('.button').css("display","none");
            }
            var list_num = METUI['$uicss'].find('.swiper-container').data("num");
            var trues = $(".$uicss .swiper-slide").length <= list_num && $(".$uicss .swiper-slide").length <= METUI['$uicss'].find('.swiper-container').data("listnum");
            var trues = trues?false:true;
            var swiper = new Swiper('.$uicss .swiper-container', {
                slidesPerView:list_num,
                centeredSlides : true,
                // slidesPerGroup :3,
                spaceBetween:10,
                autoplay:false,
                speed:500,
                spaceBetween:0,
                grabCursor: true,
                simulateTouch : true,
                centeredSlides: true,
                loop: trues,
                touchRatio : 0.5,
                navigation: {
                    nextEl: '.button-next',
                    prevEl: '.button-prev',
                },
            });
        }

    }
};
var x = new metui(METUI_FUN['$uicss']);
