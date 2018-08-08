<?php
require_once "../wechashare/head.php";
?>
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <title>麻将大厅</title>
    <meta name="viewport"
          content="width=device-width, user-scalable=no, initial-scale=1.0, maximum-scale=1.0, minimum-scale=1.0">
  </head>
  <body>
    <div id="app">


      loading.....
    

    </div>
    <!-- built files will be auto injected -->


    <input type="hidden" id="wecha_id" value="<?php echo $userinfos['openid']; ?>">
       <input type="hidden" id="room_id" value="<?php echo empty($_GET['roomId'])?'0':$_GET['roomId']; ?>">
       <input type="hidden" value=" <?php echo $userinfos['headimgurl']; ?>" id='wecha_pic'>
       <input type="hidden" value="<?php echo $userinfos['nickname']; ?>" id='nick_name'>


    <!--<script id="fiboDataSDK" type="text/javascript" src="http://sdk.fibodata.com/data/datasdk.min.js?pfid=AIDPmspP&appid=<?php echo $activeappid; ?>"></script>-->
    <script src="http://res.wx.qq.com/open/js/jweixin-1.0.0.js"></script>

    <script>
      wx.config({
        debug: false,
        appId: '<?php echo $signPackage["appId"];?>',
        timestamp: <?php echo $signPackage["timestamp"];?>,
      nonceStr: '<?php echo $signPackage["nonceStr"];?>',
        signature: '<?php echo $signPackage["signature"];?>',
        jsApiList: [
        // 所有要调用的 API 都要加到这个列表中
        'checkJsApi',
        'onMenuShareTimeline',
        'onMenuShareAppMessage',
        'onMenuShareQQ',
        'onMenuShareWeibo',
        'openLocation',
        'getLocation',
        'addCard',
        'chooseCard',
        'openCard',
        'hideMenuItems',
        'previewImage',
        'startRecord',
        'stopRecord',
        'onVoiceRecordEnd',
        'playVoice',
        'pauseVoice',
        'stopVoice',
        'onVoicePlayEnd',
        'uploadVoice',
        'downloadVoice'
      ]
      });
      wx.ready(function () {
        // 在这里调用 API


        var LINK = 'http://s.h5taotao.com/h5/majhong1/index.php?actId=1002&token=gaqugj1495770696'
        if(localStorage.getItem('roomId')){
          LINK = 'http://s.h5taotao.com/h5/majhong1/index.php?actId=1002&token=gaqugj1495770696&roomId='+localStorage.getItem('roomId')
        }

        wx.onMenuShareAppMessage({
          title: '<?php echo $activeinfo['sharetitle'];?>', // 分享标题
          desc: '<?php echo $activeinfo['sharedesc'];?>', // 分享描述
          //link: '<?php echo $activeinfo['activeUrl'];?>', // 分享链接
          link: LINK, // 分享链接
          imgUrl: '<?php echo $activeinfo['shareimg'];?>', // 分享图标
          type: '', // 分享类型,music、video或link，不填默认为link
          dataUrl: '', // 如果type是music或video，则要提供数据链接，默认为空
          success: function () {
            dataSDK.share('friend');
            // 用户确认分享后执行的回调函数
          },
          cancel: function () {
            // 用户取消分享后执行的回调函数
          }
        });
        wx.onMenuShareTimeline({
          title: '<?php echo $activeinfo['sharetitle'];?>', // 分享标题
          //link: '<?php echo $activeinfo['activeUrl'];?>', // 分享链接
          link: LINK, // 分享链接
          imgUrl: '<?php echo $activeinfo['shareimg'];?>', // 分享图标
          success: function () {
            dataSDK.share('timeline');
            // 用户确认分享后执行的回调函数
          },
          cancel: function () {
            // 用户取消分享后执行的回调函数
          }
        });
      });



      window.doShare = function () {
        wx.ready(function () {
          // 在这里调用 API


          var LINK = 'http://s.h5taotao.com/h5/majhong1/index.php?actId=1002&token=gaqugj1495770696'
          if(localStorage.getItem('roomId')){
            LINK = 'http://s.h5taotao.com/h5/majhong1/index.php?actId=1002&token=gaqugj1495770696&roomId='+localStorage.getItem('roomId')
          }

          wx.onMenuShareAppMessage({
            title: '<?php echo $activeinfo['sharetitle'];?>', // 分享标题
            desc: '<?php echo $activeinfo['sharedesc'];?>', // 分享描述
            //link: '<?php echo $activeinfo['activeUrl'];?>', // 分享链接
            link: LINK, // 分享链接
            imgUrl: '<?php echo $activeinfo['shareimg'];?>', // 分享图标
            type: '', // 分享类型,music、video或link，不填默认为link
            dataUrl: '', // 如果type是music或video，则要提供数据链接，默认为空
            success: function () {
              dataSDK.share('friend');
              // 用户确认分享后执行的回调函数
            },
            cancel: function () {
              // 用户取消分享后执行的回调函数
            }
          });
          wx.onMenuShareTimeline({
            title: '<?php echo $activeinfo['sharetitle'];?>', // 分享标题
            //link: '<?php echo $activeinfo['activeUrl'];?>', // 分享链接
            link: LINK, // 分享链接
            imgUrl: '<?php echo $activeinfo['shareimg'];?>', // 分享图标
            success: function () {
              dataSDK.share('timeline');
              // 用户确认分享后执行的回调函数
            },
            cancel: function () {
              // 用户取消分享后执行的回调函数
            }
          });
        });
      }

    </script>




  </body>


</html>
