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
        
        function init() {
            Lslide      = $('.Lslide'),//文本框对象集合
            Rslide      = $('.Rslide'),//图片框对象集合
            slideHeight = $('.wrapper').height(),//初始化的图片宽度
            index = 0;
            slideHeight = $('.slideshow-left').height();
            for (i = 0; i < Lslide.length; i++) {
                Lslide.eq(i).css("top",-slideHeight * i + "px");
                Rslide.eq(i).css("top",slideHeight * i + "px");
            }  
        }
        init();
        // 实时监听屏幕宽度，当屏幕变化试执行
        window.addEventListener('resize', function(){
            init();
            $(".$uicss .control-bottom i").removeClass('fa-circle-o').addClass('fa-arrow-up');
            $(".$uicss .control-top i").removeClass('fa-arrow-up').addClass('fa-circle-o');

        });
        function moveToTop(){
            index++; 
            if (index > Lslide.length-1) {
                index = Lslide.length-1;//到底后不执行滚动
            }else{
                for (el = 0; el < Lslide.length; el++) {
                    if (index > Lslide.length-2) {
                        $(".$uicss .control-bottom i").removeClass('fa-arrow-up').addClass('fa-circle-o');
                    }
                    $(".$uicss .control-top i").removeClass('fa-circle-o').addClass('fa-arrow-up');
                    Lslide.eq(el).css("top",parseInt(Lslide[el].style.top) + slideHeight + "px");//左侧的文本框偏移
                    Rslide.eq(el).css("top",parseInt(Rslide[el].style.top) - slideHeight + "px");//右侧的图片框偏移
                }
            }
        }
        function moveToBottom() {
            index--;
            if (index < 0) {
                index = 0;//到底后不执行滚动
            }else{
                for (el = 0; el < Lslide.length; el++) {
                    if (index < 1) {
                        $(".$uicss .control-top i").removeClass('fa-arrow-up').addClass('fa-circle-o');
                    }
                    $(".$uicss .control-bottom i").removeClass('fa-circle-o').addClass('fa-arrow-down')
                    Lslide[el].style.top = parseInt(Lslide[el].style.top) - slideHeight + "px";//左侧的文本框偏移
                    Rslide[el].style.top = parseInt(Rslide[el].style.top) + slideHeight + "px";//右侧的图片框偏移
                }
            }
        }
        //切换事件
        $(".$uicss .control-top").on("click",function(){
             moveToBottom();
        });
        $(".$uicss .control-bottom").on("click",function(){
             moveToTop();
        });
    }
};
var x = new metui(METUI_FUN['$uicss']);
