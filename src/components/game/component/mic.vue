<template>

  <div>
    <div class="mic">
      <div class="tip" v-show="showTip">
        点击发送
      </div>
      <a href="javascript:;" v-on:touchstart.stop.prevent="startRecord">
        <img src="../../../assets/new/mic.png" alt="">
      </a>
      <a href="javascript:;" @click="showEmojiFn">
        <img src="../../../assets/nav_icon/emoji.png" alt="">
      </a>
    </div>

    <div class="emoji " v-show="showEmoji.show">
      <a href="javascript:;" class="close" @click="closeEmoji">
        <img src="../../../assets/new/back.png" alt="">
      </a>
      <span v-for="(n,index) in imgs" @click="doEmoji(index)">
        <img :src="n" alt="">
      </span>
    </div>
  </div>

</template>

<script>
  import {mapGetters} from 'vuex'
  import brg from '../../../utils/middle'

  export default {
    name: 'mic',
    data(){
      return {
        imgl: 18,
        showTip: false,
        doRecord:true
      }
    },
    mounted(){
      /**
       * 返回声音
       */
      brg.$on('protoMsg', (obj) => {
        let {id, msg} = obj;
        if (id == 509){
          if (msg.type == 3&&msg.fromid!==this.user_info.id) {
            if (typeof(window.wx) != 'undefined') {
              window.wx.downloadVoice({
                serverId: msg.content, // 需要下载的音频的服务器端ID，由uploadVoice接口获得
                isShowProgressTips: 1, // 默认为1，显示进度提示
                success: function (res) {
                  window.wx.playVoice({
                    localId:  res.localId
                  });
                }
              });
            }
          }
        }


      })

    },
    methods: {
      /**
       * 上传录音
       */
      endRecord(e){
        e.preventDefault()
        let that = this
        if (typeof(window.wx) != 'undefined') {
          window.wx.stopRecord({
            success: function (res) {
              window.wx.uploadVoice({
                localId: res.localId, // 需要上传的音频的本地ID，由stopRecord接口获得
                isShowProgressTips: 1, // 默认为1，显示进度提示
                success: function (res1) {
                  that.socket.send(508, {
                    type: 3,
                    content: res1.serverId,
                    toid: that.user_info.id
                  })
                }
              });
            }
          });
        }
      },
      /**
       * 开启录音
       */
      startRecord(e){
        if (typeof(window.wx) !== 'undefined') {
            if(this.doRecord){
              wx.startRecord();
              this.doRecord = false
              this.showTip = true

            }else{
              this.showTip = false
              this.doRecord = true
              this.endRecord(e)
            }

        }
      },
      showEmojiFn(){
        this.$store.dispatch("showEmoji", {show: true})
      },
      closeEmoji(){
        this.$store.dispatch("showEmoji", {show: false})
      },
      doEmoji(index){
        this.socket.send(508, {
          type: 1,
          content: index + '',
          toid: this.user_info.id
        })
      }

    },
    computed: {
      ...
        mapGetters({
          'showEmoji': 'showEmoji',
          'user_info': 'user_info',

        }),
      imgs()
      {
        let obj = []

        for (let i = 1; i <= this.imgl; i++) {
          let img = require(`../../../assets/icon/emoji/3/${i}.png`)
          obj.push(img)
        }
        return obj
      }
    }
  }

</script>

<style scoped lang="scss">
  .animated {
    animation-duration: .3s;
  }

  .mic {
    position: fixed;
    top: 0;
    left: 54%;
    -webkit-transform: rotate(-90deg);
    -moz-transform: rotate(-90deg);
    -ms-transform: rotate(-90deg);
    -o-transform: rotate(-90deg);
    transform: rotate(-90deg);
    z-index: 1000;
    img {
      display: block;
      margin-top: .2rem;
      width: .6rem;
    }
    .tip {
      position: absolute;
      left: -.5rem;
      top: -.2rem;
      width: 1.5rem;
      font-size: .2rem;
      background: rgba(0, 0, 0, .3);
      padding: .1rem;
    }

  }

  .emoji {
    position: fixed;
    top: 50%;
    left: 50%;
    -webkit-transform: translate(-50%, -50%) rotate(-90deg);
    -moz-transform: translate(-50%, -50%) rotate(-90deg);
    -ms-transform: translate(-50%, -50%) rotate(-90deg);
    -o-transform: translate(-50%, -50%) rotate(-90deg);
    transform: translate(-50%, -50%) rotate(-90deg);
    z-index: 10000;
    width: 6.43rem;
    height: 3.46rem;
    border: .08rem solid #09a354;
    border-radius: .26rem;
    background: #043a28;
    padding: .1rem 0;
    z-index: 999999999;

    .close {
      position: absolute;
      top: -.3rem;
      right: -.3rem;
      width: .6rem;
      height: .6rem;
      border-radius: .3rem;
      img {
        width: 100%;
      }
    }

    span {
      display: inline-block;
      width: 1rem;
      height: 1rem;
      overflow: hidden;
      img {
        margin-top: .1rem;
        width: .86rem;
      }
    }

  }


</style>
