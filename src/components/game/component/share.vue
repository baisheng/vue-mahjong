<template>

  <div>
    <div class="share" v-show="sharetip.show">
      <span class="back" @click="closeTip">
        <img src="../../../assets/new/back.png" alt="">
      </span>
      <div class="tap" flex="main:center cross:center box:mean">
        <div class="item active" flex="main:center cross:center box:mean" @click="qr=false">
          链接邀请
        </div>
        <div class="item" flex="main:center cross:center box:mean" @click="qr=true">
          扫码邀请
        </div>
      </div>

      <div class="con1" v-show="!qr">
        <div class="img">
          <img src="../../../assets/new/step.png" alt="">
        </div>

        <div class="share_tip">
          <img :src="myself.wecha_pic" alt="">
          <p>{{myself.nick_name}}看对接房间号{{room_id}}</p>
        </div>
        <div class="img">
          <img src="../../../assets/new/step2.png" alt="">
        </div>

      </div>
      <div class="con2" v-show="qr">
        <img :src="myself.wecha_pic" alt="">
        <p>
          扫描二维码<br>
          直接进入房间
        </p>
      </div>
    </div>
    <div class="bg">

    </div>
  </div>

</template>

<script>
  import {mapGetters} from 'vuex'

  export default {
    data() {
      return {
        qr: false
      }
    },
    mounted() {

    },
    methods: {
      closeTip() {


        this.$store.dispatch('showShareModal', {
          ...this.sharetip,
          show: false
        })
      }
    },
    computed: {
      ...mapGetters({
        'sharetip': 'sharetip',
        'room_id':'room_id'
      })
    }

  }

</script>

<style scoped lang="scss">
  .share {
    position: fixed;
    top: 50%;
    left: 50%;
    margin-top: -2.46rem;
    margin-left: -2.045rem;
    -webkit-transform: rotate(-90deg);
    -moz-transform: rotate(-90deg);
    -ms-transform: rotate(-90deg);
    -o-transform: rotate(-90deg);
    transform: rotate(-90deg);

    width: 4.09rem;
    height: 4.86rem;
    border: 2px solid #11a29e;
    z-index: 1000000;
    background: #0a332f;
    border-radius: .2rem;

    .back {
      position: absolute;
      top: -.25rem;
      right: -.25rem;
      img {
        width: .5rem;
        height: .5rem;
      }
    }

    .tap {
      margin: .2rem auto;
      width: 80%;
      border: 1px solid #2bb488;
      border-radius: .1rem;
      font-size: .24rem;
      height: .48rem;
      .item {
        height: .48rem;
        width: 100%;
        &.active {
          background: #2bb488;
          color: #fff;
        }
      }
    }
    .con2 {
      img {
        margin-top: .5rem;
        border: 1px solid #1d865d;
        width: 1.8rem;
        height: 1.8rem;
      }
      p {
        margin-top: .2rem;
        font-size: .22rem;
      }
    }
    .con1 {
      .img {
        margin: 0 auto;
        width: 3.2rem;
        img {
          width: 100%;
        }
      }
      .share_tip {
        margin: .2rem auto;
        text-align: left;
        width: 3.16rem;
        height: 1.48rem;
        overflow: auto;
        border: 1px solid #1d865d;
        border-radius: 4px;
        padding: .1rem;
        img {
          float: left;
          width: .64rem;
          height: .64rem;
          margin: .2rem;
        }
        p {
          font-size: .2rem;

        }
      }

    }
  }

</style>
