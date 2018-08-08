<template>
  <div class="nav" flex="cross:center box:mean">
    <div class="item" flex="main:center cross:center">
      <router-link to="/recharge" >
      <!--  <img src="../../assets/recharge.png" alt="">
        <span>充值</span>-->
      </router-link>
    </div>
    <div class="item" flex="main:center cross:center">
      <router-link to="/rank" replace >
       <!-- <img src="../../assets/rank.png" alt="">
        <span>战绩</span>-->
      </router-link>
    </div>
    <div class="item da" flex="main:center cross:center">
      <router-link to="/room2" replace >
       <!-- <img src="../../assets/game.png" alt="">
        <span>游戏大厅</span>-->
      </router-link>

    </div>
    <div class="item" flex="main:center cross:center">
      <router-link to="/discover" replace >
        <!--<img src="../../assets/discover.png" alt="">
        <span>发现</span>-->
      </router-link>

    </div>

    <div class="item" flex="main:center cross:center">
      <router-link to="/person" >
       <!-- <img src="../../assets/user.png" alt="">
        <span>我的</span>-->
      </router-link>
    </div>
  </div>
</template>

<script>
  export default {
    name: 'game',
    data(){
      return {}
    },
    
  }
</script>

<style scoped lang="scss">
  .nav {
    position: fixed;
    bottom: 0;
    left: 0;
    width: 100%;
    height: 1.5rem;
    color: #61f0ab;
    background: url('../../assets/nav_icon/mj.png') center center no-repeat;
    -webkit-background-size: 100%;
    background-size: 100%;
    background-position: 0 100%;




    .item {
      height: 100%;
      &:last-of-type {
        border: none;
      }
      a {
        display: block;
        padding-top: .1rem;
        height: 100%;
        text-decoration: none;
        color: #61f0ab;
        font-size: .32rem;
        width: 100%;
        img{
          width: .4rem;
          height: .4rem;
        }
        span{
          margin-top: .1rem;
          font-size: .22rem;
        }

      }
      &.da{
        width: .5rem;
      }
    }
  }
</style>
