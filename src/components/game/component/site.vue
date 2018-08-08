<template>

  <div class="sitem" v-show="showSite.show">
    <div :class='["site_pan",siteC]'>
      <img :src="light" alt="">
    </div>
    <div class="time">
      {{time}}
    </div>
    <div class="leave">
      剩余：<span>{{leave}}</span>
    </div>
  </div>

</template>

<script>
  import {mapGetters} from 'vuex'


  export default {
    name: 'info',
    props: [ 'time', 'leave'],
    data() {
      return {}
    },
    mounted() {


    },
    computed: {
      ...mapGetters({
        'members': 'members',
        'touzistatus': 'touzistatus',
        'showSite': 'showSite',
        'user_info': 'user_info',
        'all_users': 'all_users',
        'showstart': 'showstart',
      }),

      showSite1() {
        let temp = 0;
        let all =Object.assign({},this.all_users);
        for(let a in all){
          if(all[a].position){
            temp++;
          }
        }
        if (temp >= 4) {
          return true
        } else {
          return false
        }
      },
      light() {
        let opos = this.showSite.position
        //找到出牌位置的风圈
        let all =Object.assign({},this.all_users);
        let aa = 1
        for(let a in all){
          if(all[a].position==opos){
            aa = all[a]['order']
          }
        }

        let str = require(`../../../assets/nav_icon/site/${aa}.png`)
        return str

      },

      siteC() {
        let me = this.all_users.user1Data

        if (typeof(me) === 'undefined') {
          return false
        }

        return 'site' + me.order


      },

    },
    methods: {}
  }

</script>

<style scoped lang="scss">
  .animated {
    animation-duration: .3s;
  }

  .time {
    position: fixed;
    top: 50%;
    left: 50%;
    -webkit-transform: translate(-50%, -50%) rotate(-90deg);
    -moz-transform: translate(-50%, -50%) rotate(-90deg);
    -ms-transform: translate(-50%, -50%) rotate(-90deg);
    -o-transform: translate(-50%, -50%) rotate(-90deg);
    transform: translate(-50%, -50%) rotate(-90deg);
    z-index: 0;
    font-size: .4rem;
    width: .7rem;
    height: .7rem;
    line-height: .68rem;
    background: rgba(0, 0, 0, .7);
    color: #fff;
    font-weight: 800;
    text-align: center;

  }

  .leave {
    position: fixed;
    top: 44%;
    left: 48%;
    -webkit-transform-origin: 0 0;
    -moz-transform-origin: 0 0;
    -ms-transform-origin: 0 0;
    -o-transform-origin: 0 0;
    transform-origin: 0 0;
    -webkit-transform: rotate(-90deg);
    -moz-transform: rotate(-90deg);
    -ms-transform: rotate(-90deg);
    -o-transform: rotate(-90deg);
    transform: rotate(-90deg);
    color: #ffb400;
    font-size: .2rem;
    span {
      font-size: .38rem;
      font-weight: 600;
      position: relative;
      top: .05rem;
    }
  }

  .site_pan {
    z-index: 10;
    position: fixed;
    top: 50%;
    left: 50%;
    width: 1.4rem;
    height: 1.4rem;
    margin-left: -.7rem;
    margin-top: -.7rem;

    img {
      width: 1.4rem;
      height: 1.4rem;
    }

    &.site1 {
      -webkit-transform: rotate(-90deg);
      -moz-transform: rotate(-90deg);
      -ms-transform: rotate(-90deg);
      -o-transform: rotate(-90deg);
      transform: rotate(-90deg);
    }
    &.site2 {
      -webkit-transform: rotate(-360deg);
      -moz-transform: rotate(-360deg);
      -ms-transform: rotate(-360deg);
      -o-transform: rotate(-360deg);
      transform: rotate(-360deg);
    }
    &.site3 {
      -webkit-transform: rotate(-270deg);
      -moz-transform: rotate(-270deg);
      -ms-transform: rotate(-270deg);
      -o-transform: rotate(-270deg);
      transform: rotate(-270deg);
    }
    &.site4 {
      -webkit-transform: rotate(-180deg);
      -moz-transform: rotate(-180deg);
      -ms-transform: rotate(-180deg);
      -o-transform: rotate(-180deg);
      transform: rotate(-180deg);
    }

  }

</style>
