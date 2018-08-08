<template>

  <div v-show="loading.show" class="loading" flex="main:center cross:center dir:top">
    <img src="../../../assets/nav_icon/logo.png" alt="" class="logo">
    <div class="l">
      loading...
    </div>
    <div class="text">
      <p>抵制不良游戏<span></span>拒绝盗版游戏</p>
      <p>注意自我保护<span></span>谨防受骗上当</p>
      <p>适度游戏益脑<span></span>沉迷游戏伤身</p>
      <p>合理安排时间<span></span>享受健康生活</p>
    </div>
  </div>
</template>


<script>
  import {mapGetters} from 'vuex'

  export default {
    data() {
      return {}
    },
    mounted() {

    },
    created() {

    },
    computed: {
      ...mapGetters({
        'loading':'loading'
      })
    }
  }
</script>

<style scoped lang="scss">
  .loading {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background: #012118;
    z-index: 99999999992;
    color: #219555;

    .logo{
      width: 2.5rem;
      margin-bottom: 1.5rem;
    }
    .text{
      color: #219555;
      position: fixed;
      left: 50%;
      -webkit-transform: translateX(-50%);
      -moz-transform: translateX(-50%);
      -ms-transform: translateX(-50%);
      -o-transform: translateX(-50%);
      transform: translateX(-50%);
      width: 90%;
      text-align: center;
      bottom: 30px;
      font-size: .28rem;
      line-height: 2;
      span{
        display: inline-block;
        width: .4rem;
      }
    }

  }
</style>
