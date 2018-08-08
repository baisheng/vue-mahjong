<template>

  <div>
    <div class="room_msg" v-show="isShowMsg">
      <div class="title" flex="main:center cross:center">
        消息列表
      </div>

      <div class="msg_list">
        <div class="item" v-for="n in members" flex="main:center cross:center box:first">
          <img :src="n.iconUrl" alt=""><div> {{n.playerName}}进入了房间</div>
        </div>
      </div>


    </div>
    <div class="bg" v-show="isShowMsg" @click="hideMsg">

    </div>
  </div>

</template>

<script>
  import {mapGetters} from 'vuex'

  export default {
    name: 'zhanji',

    data() {
      return {}
    },
    mounted() {


    },
    methods: {
      hideMsg() {
        this.$store.dispatch('showRMsg', false)
      }
    },
    computed: {
      ...mapGetters({
        'isShowMsg': 'isShowMsg',
        'members': 'members'
      })

    }
  }

</script>

<style scoped lang="scss">
  .animated {
    animation-duration: .3s;
  }

  .bg {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    z-index: 99999999999;
    border: 1px solid #000;
  }

  .room_msg {
    position: fixed;
    bottom: 0;
    right: -3.8rem;
    width: 3.8rem;
    height: 6.6rem;
    -webkit-transform-origin: 0 100%;
    -moz-transform-origin: 0 100%;
    -ms-transform-origin: 0 100%;
    -o-transform-origin: 0 100%;
    transform-origin: 0 100%;
    -webkit-transform: rotate(-90deg);
    -moz-transform: rotate(-90deg);
    -ms-transform: rotate(-90deg);
    -o-transform: rotate(-90deg);
    transform: rotate(-90deg);
    background: #00302a;
    z-index: 1000;
    font-size: .24rem;
    box-shadow: 0 0 10px rgba(0, 0, 0, .4);
    .title {
      background: #0b4e40;
      height: .54rem;
      font-size: .28rem;
    }
    .msg_list {
      padding: .2rem;
      text-align: left;
      font-size: .2rem;
      .item {
        padding: .1rem 0;
        img{
          width: .6rem;
          height: .6rem;
          border-radius: .3rem;
          margin-right: .2rem;
          border: 1px solid #ffb400;
        }
        border-bottom: 1px solid #00302a;
      }
    }

  }


</style>
