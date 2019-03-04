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
        $(".$uicss").scrollFun(function(val){
            $(".$uicss .wrapper-box").removeClass("loading");
        });
        var b=METUI['$uicss'].find('.btn-cntotc');
        b.on('click', function(event) {
             var lang=$(this).attr('data-tolang');
             if (lang=='tc') {
                $('body').s2t();
                $(this).attr('data-tolang', 'cn');
                $(this).text('简体');
             } else if(lang=='cn') {
                $('body').t2s();
                $(this).attr('data-tolang', 'tc');
                $(this).text('繁體');
             }
          });
        //自定义方法，调用方式可写在导航的回调里面来实现滚动到对应的屏幕执行对应的方法
        (function(){
            $.fn.extend({
                pullpage_foot_fun:function(num){
                    $(".$uicss .wrapper-box").removeClass("loading");
                }
              });
        })(jQuery);
        //自定义方法
    }
};
var x = new metui(METUI_FUN['$uicss']);
