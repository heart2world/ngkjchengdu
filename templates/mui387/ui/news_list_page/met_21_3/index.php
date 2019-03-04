<?php defined('IN_MET') or exit('No permission'); ?>
<?php $sidebar=strlen($ui[has][sidebar]);?>
<main class="$uicss met-news" m-id='{$ui.mid}'>
    <div class="container">
        <div class="row">
        <div class="news-title-bor">
            <div class="container">
                <div class="news-title-con">
                    <tag action="category" type="current" cid="$data[classnow]">
                        <span class="news-top">{$m.name}</span>
                        <p class="news-desc">{$m.description}</p>
                        <span class="news-hr"></span>
                        <p class="news-xs">{$m.namemark}</p>
                    </tag>
                </div>
            </div>
        </div>

 <if value="$data[index_num] eq 7 && $data[sub]">
<!--展示下级栏目-->
    <div class="$uicss"  m-id='{$ui.mid}'>
        <ul class="service-list blocks-100 blocks-sm-2 blocks-md-3 blocks-xlg-4  clearfix">
            <tag action="category" type="son" cid="$data[classnow]">
                <li class="item">
	                <a href="{$m.url}" title="{$m.name}" {$m.urlnew}>
		                <img src="{$m.columnimg}" alt="{$m.name}"/>
		                <h4>{$m.name}</h4>
		                <p>{$m.description}</p>
	                </a>
                </li>
            </tag>
        </ul>
    </div>
<else/>
    <tag action='news.list' num="$c['met_news_list']"></tag>
        <if value="$sub">
            <if value="$_M['form']['pageset']">
                <if value="$sidebar">
                        <div class="col-md-9 met-news-body">
                            <div class="row">
                    </if>
                <else/>
                    <if value="$ui[has][sidebar]">
                        <div class="col-md-9 met-news-body">
                            <div class="row">
                    </if>
                </if>
            <if value="$ui['news_headlines'] && $ui['news_listtype'] neq 3">
<!--头条-->
                <if value="!$data[page] && !$data[class2]">
    	           <div class="news-headlines imagesize" data-scale='{$ui.news_headlines_y}x{$ui.news_headlines_x}'>
    	               <div class="news-headlines-slick cover">
                	        <tag action='news.list' num="$ui[news_headlines_num]" cid="$data[classnow]">
                	            <div class='slick-slide'>
                	                <a href="{$v.url}" title="{$v.title}" {$g.urlnew}>
                	                    <img class="cover-image" data-lazy="{$v.imgurl|thumb:$ui['news_headlines_x'],$ui['news_headlines_y']}" data-srcset="{$v.imgurl|thumb:450,450*$ui['news_headlines_y']/$ui['news_headlines_x']} 450w,{$v.imgurl|thumb:$ui['news_headlines_x'],$ui['news_headlines_y']}" sizes='(max-width:479px) 450px' alt="{$v.title}">
                	                    <div class="headlines-text text-xs-center">
                	                        <h3>{$v.title}</h3>
                	                    </div>
                	                </a>
                	            </div>
                	        </tag>
    	               </div>
    	           </div>
                </if>
            </if>
            <div class="met-news-list">
                <ul class="ulstyle met-pager-ajax imagesize invisible" data-scale='{$c.met_newsimg_y}x{$c.met_newsimg_x}' m-id='{$ui.mid}' data-plugin="appear" data-animate="slide-bottom50" data-repeat="false">
                    <tag action='news.list' num="$c['met_news_list']" cid="$data[classnow]">  
                    <if value="$ui['news_listtype'] eq 4">
                        <if value="($ui['news_headlines'] && !$data[page] && !$data[class2] && $v['_index'] egt $ui[news_headlines_num]) || ($ui['news_headlines'] && !$data[page] && $data[class2]) || $data[page] || !$ui['news_headlines']">
                        <?php
                            $time = explode('-',$v['updatetime']);
                            $time_year = $time[0];
                            $time_month = $time[1];
                            $time_day = $time[2];
                            switch ($time_month) {
                                case "01":
                                    $time_month = "January";
                                    break;
                                case "02":
                                    $time_month = "February";
                                    break;
                                case "03":
                                    $time_month = "March";
                                    break;
                                case "04":
                                    $time_month = "April";
                                    break;
                                case "05":
                                    $time_month = "May";
                                    break;
                                case "06":
                                    $time_month = "June";
                                    break;
                                case "07":
                                    $time_month = "July";
                                    break;
                                case "08":
                                    $time_month = "August";
                                    break;
                                case "09":
                                    $time_month = "September";
                                    break;
                                case "10":
                                    $time_month = "October";
                                    break;
                                case "11":
                                    $time_month = "November";
                                    break;
                                case "12":
                                    $time_month = "December";
                                    break;
                               default:
                                 echo "null";
                            }
                        ?>
                                <li class='border-bottom1 news-jdf '>
                                    <div class="col-sm-3 news-left">

                                        <p class="time-day">{$time_day}</p>
                                        <p class="time-my">
                                            <span>{$time_month}</span>&nbsp;&nbsp;
                                            <span>{$time_year}</span>
                                        </p>
                                        <span class="time-hr"></span>
                                        <p  class="time-desc">{$v.issue}</p>
                                    </div>
                                    <div class="news-right col-sm-9">
                                        <div class="news-img">
                                            <img src="{$v.imgurl|thumb:$ui['news_right_w'],$ui['news_right_h']}" alt="">
                                        </div>
                                        <div class="news-text">
                                            <h4 class="news-title">{$v.title}</h4>
                                            <span class="news-right-hr"></span>
                                            <p class="news-desc">{$v.description}</p>
                                            <a href="{$v.url}" class="news-btn">{$ui.more}</a>
                                        </div>
                                    </div>
                                </li>
                        </if>
                    </if>
                        <if value="$ui['news_listtype'] eq 1">
<!-- 极简模式 -->
                            <if value="($ui['news_headlines'] && !$data[page] && !$data[class2] && $v['_index'] egt $ui[news_headlines_num]) || ($ui['news_headlines'] && !$data[page] && $data[class2]) || $data[page] || !$ui['news_headlines']">
                                <li class='border-bottom1'>
                                    <h4>
                                        <a href="{$v.url}" title="{$v.title}" {$g.urlnew}>{$v.title}</a>
                                    </h4>
                                    <div class="time">
                            			<span class="author">{$v.issue} </span>
                            			<span class="times">{$v.original_addtime} {$ui.time_txt}</span>
                            		</div>
                                    <p class="des font-weight-300">{$v.description}</p>
                                    <p class="info font-weight-300">
                                        <span><i class="icon wb-eye m-r-5 font-weight-300" aria-hidden="true"></i>{$v.hits} {$ui.hits_txt}</span>
                                    </p>
                                </li>
                            </if>
                        </if>
                        <if value="$ui['news_listtype'] eq 2">
<!-- 图文模式 -->
                            <if value="($ui['news_headlines'] && !$data[page] && !$data[class2] && $v['_index'] egt $ui[news_headlines_num]) || ($ui['news_headlines'] && !$data[page] && $data[class2]) || $data[page] || !$ui['news_headlines']">
                                <li class="media media-lg border-bottom1">
                                    <div class="media-left">
                                        <a href="{$v.url}" title="{$v.title}" {$g.urlnew}>
                                            <img class="media-object" <if value="$v['_index'] gt ($ui['news_headlines']?2+$ui['news_headlines_num']:3) || $data['page'] gt 1">data-original<else/>src</if>="{$v.imgurl|thumb:$c['met_newsimg_x'],$c['met_newsimg_y']}" alt="{$v.title}" height='100'>
                                        </a>
                                    </div>
                                    <div class="media-body">
                                        <h4>
                                            <a href="{$v.url}" title="{$v.title}" {$g.urlnew}>{$v.title}</a>
                                        </h4>
                                        <div class="time">
                            				<span class="author">{$v.issue} </span>
                            				<span class="times">{$v.original_addtime} {$ui.time_txt}</span>
                            			</div>
                                        <p class="des font-weight-300">
                                            {$v.description}
                                        </p>
                                    <p class="info font-weight-300">
                                        <span><i class="icon wb-eye m-r-5 font-weight-300" aria-hidden="true"></i>{$v.hits} {$ui.hits_txt}</span>
                                    </p>
                                    </div>
                                </li>
                            </if>
                        </if>
                        <if value="$ui['news_listtype'] eq 3">
<!-- 橱窗模式 -->
                        	<li class="card card-shadow">
                        	    <div class="card-header p-0">
                        	        <a href="{$v.url}" title="{$v.title}" {$g.urlnew}>
                        	            <img class="cover-image" <if value="$v['_index'] gt 3 || $data['page'] gt 1">data-original<else/>src</if>="{$v.imgurl|thumb:$ui['news_ccimg_x'],$ui['news_ccimg_y']}" alt="{$v.title}" height='100'>
                        	        </a>
                        	    </div>
                        	    <div class="card-body">
                        	        <h4 class="card-title">
                        	            <a href="{$v.url}" title="{$v.title}" {$g.urlnew}>{$v.title}</a>
                        	        </h4>
                        	        <div class="time">
                        				<span class="author">{$v.issue} </span>
                        				<span class="times">{$v.original_addtime} {$ui.time_txt}</span>
                        			</div>
                        	        <p class=" font-weight-300">{$v.description}</p>
                        	        <p class="card-metas font-size-12 font-weight-300 info">
                        	            <span><i class="icon wb-eye m-r-5 font-weight-300" aria-hidden="true"></i>{$v.hits} {$ui.hits_txt}</span>
                        	        </p>
                        	    </div>
                        	</li>
                        </if>
                    </tag>
                    
                </ul>
                        <div class='m-t-20 text-xs-center hidden-sm-down' m-type="nosysdata">
                            <pager/>
                        </div>
                        <div class="met_pager met-pager-ajax-link hidden-md-up" data-plugin="appear" data-animate="slide-bottom" data-repeat="false" m-type="nosysdata">
                            <button type="button" class="btn btn-primary btn-block btn-squared ladda-button" id="met-pager-btn" data-plugin="ladda" data-style="slide-left" data-url="" data-page="1">
                                <i class="icon wb-chevron-down m-r-5" aria-hidden="true"></i>
                            </button>
                        </div>
                    </div>
        <else/>
                    <div class='h-100 text-xs-center font-size-20 vertical-align' m-id='{$ui.mid}'>{$ui.nodata}</div>
        </if>
</if>
<if value="$_M['form']['pageset']">
    <if value="$sidebar">
            </div>
        </div>
    <else/>
                </div>
            </div>
        </main>
    </if>
<else/>
    <if value="$ui[has][sidebar]">
            </div>
        </div>
    <else/>
            </div>
        </div>
    </main>
    </if>
</if>
