METUI_FUN['$uicss'] = {
    name: '$uicss',
    appear: function() {
        // 返回顶部按钮显示/隐藏
        $(window).scroll(function(){
            if($(this).scrollTop()>$(this).height()){
                $(".met-scroll-top").removeAttr('hidden').addClass("animation-slide-bottom");
            }else{
                $(".met-scroll-top").attr({hidden:''}).removeClass('animation-slide-bottom');
            }
        });
        $(function(){
            $(".$uicss-loader-box").fadeOut(1000);
        });
        var fullpage_ok = $(".head_nav_met_21_14-pusher").data("fullpage"),
            fullpage_list_array = $("#fullpage").find('.section'),
            fullpage_type=$(".$uicss-pusher").data("scroll"),
            fullpage_nav_array = new Array(),
            fullpage_right_array = new Array();
                if(fullpage_ok=="ok" && fullpage_type){
                    fullpage_list_array.each(function(index, el) {
                        var pullpage_nav_id = $(this).data("id"),
                            pullpage_nav_name = $(this).data("name"),
                            pullpage_nav_mid = $(this).attr("m-id"),
                            pullpage_active_index = index == 0?"active":" ";
                        $("#menu .nav-top").append("<li role='presentation' data-menuanchor='"+pullpage_nav_id+"' class='menu__item "+pullpage_active_index+"' m-id="+pullpage_nav_mid+"><a class='menu__item-name' href='#"+pullpage_nav_id+"'>"+pullpage_nav_name+"</a></li>");
                        fullpage_nav_array.push(pullpage_nav_id);
                        fullpage_right_array.push(pullpage_nav_name);
                    });
                    fullpage_mod_ok = true;
                }else{
                    fullpage_nav_array = false;
                }
                
            if(fullpage_ok=="ok" && fullpage_type){
                var Mask = $(".$uicss-pusher").data("mask");
                var fullpage_new = new fullpage('.$uicss-fullpage', {
                    lockAnchors: false,
                    anchors:fullpage_nav_array,
                    menu:'#menu',
                    recordHistory:false,
                    showActiveTooltip:true,
                    afterLoad: function(origin,destination,direction){
                        if(origin){
                            // var pull_fun_id = ".section [data-Method="+$(".section[data-id="+destination['anchor']+"]").data("fun")+"]",//获取对应要执行方法的对象
                            //     pull_fun_num = $(pull_fun_id).length;//获取多个执行方法的次数
                            // if(pull_fun_num){
                            //     for (var i=0; i<pull_fun_num; i++){//如有多个方法需要调用循环执行多次
                            //         var pull_index_parameter = $(pull_fun_id).eq(i).data("parameter");//获取需要传递的参数名
                            //         $(pull_fun_id).eq(i).pullpage_fun(pull_index_parameter);//执行对应id值的方法
                            //     }
                            // }
                        }
                    },
                    onLeave: function(index, nextIndex, direction){
                        if(nextIndex['index']>0){
                            $(".met-scroll-top").removeAttr('hidden').addClass("animation-slide-bottom");
                        }else{
                            $(".met-scroll-top").attr({hidden:''}).removeClass('animation-slide-bottom');
                        }
                        var pull_fun_id = ".section [data-Method="+$(".section[data-id="+nextIndex['anchor']+"]").data("fun")+"]";//获取对应要执行方法的对象
                        if(pull_fun_id){
                            var pull_fun_name = $(".section[data-id="+nextIndex['anchor']+"]").data("funname");//获取需要执行对应的函数类型
                            var pull_fun_num = $(pull_fun_id).length;//获取多个执行方法的次数
                            for (var i=0; i<pull_fun_num; i++){//如有多个方法需要调用循环执行多次
                                var pull_index_parameter = $(pull_fun_id).eq(i).data("parameter");//获取需要传递的参数名
                                switch(pull_fun_name)
                                    {
                                    case "banner":
                                        $(pull_fun_id).eq(i).pullpage_banner_fun(pull_index_parameter);//执行对应id值的方法
                                    break;
                                    case "about":
                                        $(pull_fun_id).eq(i).pullpage_about_fun(pull_index_parameter);//执行对应id值的方法
                                    break;
                                    case "news":
                                        $(pull_fun_id).eq(i).pullpage_news_fun(pull_index_parameter);//执行对应id值的方法
                                    break;
                                    case "service":
                                        $(pull_fun_id).eq(i).pullpage_service_fun(pull_index_parameter);//执行对应id值的方法
                                    break;
                                    case "product":
                                        $(pull_fun_id).eq(i).pullpage_product_fun(pull_index_parameter);//执行对应id值的方法
                                    break;
                                    case "foot":
                                        $(pull_fun_id).eq(i).pullpage_foot_fun(pull_index_parameter);//执行对应id值的方法
                                    break;
                                    default:
                                }
                            }
                        }
                        //导航遮罩
                        if(Mask.indexOf(nextIndex['item']['dataset']['name']) >= 0){
                            $(".$uicss-top-head").css("background","rgba(0,0,0,.3)");
                        }else{
                            $(".$uicss-top-head").css("background","rgba(0,0,0,0)");
                        }
                        setTimeout(function(){
                            $(".section[data-id="+nextIndex['anchor']+"] [data-plugin=appear]").appearDiy();
                        },50);

                        $(".section[data-id="+nextIndex['anchor']+"] img.imgloading").lazyload();
                    }
                });
            }else{
                // $(".$uicss-fullpage").css("paddingTop",$(".$uicss-top-head").height()+"px");
            }
            $(".met-scroll-top").click(function(){
                // $('html,body').animate({'scrollTop':0},300);
                if(fullpage_ok=="ok"){
                    fullpage_new.moveTo();
                }else{
                     $('html,body').animate({'scrollTop':0},300);
                }
                
            });
            new mlPushMenu( document.getElementById('mp-menu'), document.getElementById('trigger') );
            // $(window).scroll(function() {
            //     $(".$uicss-top-head").css("top",$(this).scrollTop()+"px");
            // });
    },
    navtab:function(){
        // 产品列表
        var $met_indexpro_navtabs=$(".$uicss-nav-tabs .nav-tabs");
        var list_li=$met_indexpro_navtabs.find('li');
        if($met_indexpro_navtabs.length && $(window).width() <=1024){
            setTimeout(function(){
                $met_indexpro_navtabs.navtabSwiper();// 选项卡水平滚动
            },100);
            
        }
    },
    cntotc:function(){
        
        //简体繁体互换
        var b=$('.langjf-box .btn-cntotc');
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
    }
};
var x = new metui(METUI_FUN['$uicss']);