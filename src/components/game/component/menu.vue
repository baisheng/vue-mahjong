<template>

  <div>
    <div class="bg"  v-show="isShow" @click="isShow = false">

    </div>
    <div class="menu" flex="main:center cross:center" @click="isShow = !isShow">
      <img src="../../../assets/new/meun.png" alt="" class="icon-15">
    </div>
    <div class="menu-content animated fadeIn" v-show="isShow">
      <div class="item" @click="goVisit" flex="cross:center box:first">
        <img src="../../../assets/look.png" alt=""  class="icon-20" />
        站起围观
      </div>
      <div class="item" flex="cross:center box:first" @click="showtip"><img src="../../../assets/share.png" alt="" class="icon-20">邀请好友
      </div>
      <div class="item" flex="cross:center box:first" @click="goHome"><img src="../../../assets/back.png" alt=""
                                                                           class="icon-20">返回大厅
      </div>
      <div class="item" flex="cross:center box:first"><img src="../../../assets/setting.png" alt="" class="icon-20">玩家信息
      </div>
    </div>
  </div>


</template>

<script>

  import { mapGetters } from 'vuex'

  export default {
    name: 'menu',
    data() {
      return {

        isShow: false

      }
    },
    computed:{
      ...mapGetters({
        'room_id':'room_id'
      })
    },
    methods: {
      goHome() {
        this.socket.send(205)
      },
      goVisit() {

        this.socket.send(207,{
          roomId:this.room_id,
          position:0
        })


      },
      showtip(){
        this.$store.dispatch('showShareModal', {show: true})
      }

    }
  }

</script>

<style scoped lang="scss">
  .bg{
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background: rgba(0,0,0,.1);
  }
  .menu {
    position: fixed;
    bottom: 8px;
    left: 8px;
    width: .6rem;
    height: .6rem;
    border: 1px solid #2c9f82;
    border-radius: 5px;
    -webkit-transform: rotate(-90deg);
    -moz-transform: rotate(-90deg);
    -ms-transform: rotate(-90deg);
    -o-transform: rotate(-90deg);
    transform: rotate(-90deg);
    background: #022a1f;
    box-shadow: 0 0 8px rgba(0, 0, 0, .2);
    z-index: 9999999999;

    img {
      color: #fff;
      font-size: .8rem;
      font-weight: bold;

    }

  }

  .menu-content {
    position: fixed;
    bottom: 10px;
    left: .7rem;

    -webkit-transition-duration: .3s;
    -moz-transition-duration: .3s;
    -ms-transition-duration: .3s;
    -o-transition-duration: .3s;
    transition-duration: .3s;

    -webkit-transform: rotate(-90deg);
    -moz-transform: rotate(-90deg);
    -ms-transform: rotate(-90deg);
    -o-transform: rotate(-90deg);
    transform: rotate(-90deg);
    z-index: 10000;
    width: 3rem;
    border: 2px solid #2c9f82;
    border-radius: 8px;
    padding: .12rem;
    font-size: .28rem;
    background: #022a1f;
    color: #2c9f82;
    .item {
      padding: .1rem;
      padding-left: .2rem;
      img {
        color: #fff;
        margin-right: .2rem;
      }
    }
  }

  .animated {
    -webkit-animation-duration: .2s;
    -moz-animation-duration: .2s;
    -o-animation-duration: .2s;
    animation-duration: .2s;
  }

</style>
