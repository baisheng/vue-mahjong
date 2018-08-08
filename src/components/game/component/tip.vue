<template>

  <div class="tip_modal animated fadeIn" v-show="show">
    <a href="javascript:;" @click="show = false">
      <img src="../../../assets/nav_icon/back.png" alt="">
    </a>
  </div>

</template>

<script>
  export default {
    name:'info',
    props:['show'],
    data(){
      return {

      }
    },
    mounted(){


    },
    methods:{

    }
  }

</script>

<style scoped lang="scss">
  .animated{
    animation-duration: .3s;
  }
  .tip_modal{
    z-index: 1000;
    position: fixed;
    top: 29%;
    left: 30%;
    -webkit-transform: rotate(-90deg);
    -moz-transform: rotate(-90deg);
    -ms-transform: rotate(-90deg);
    -o-transform: rotate(-90deg);
    transform: rotate(-90deg);



    .bg_img{
      width: 4rem;
    }
    a{
      position: absolute;
      top: -.3rem;
      right: -.3rem;
      img{
        width: .8rem;
        height: .8rem;
      }
    }






  }

</style>
